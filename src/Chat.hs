{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Chat where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Lens ((^?))
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key)
import Data.Maybe (isJust)
import Data.String.Interpolate (i, iii)
import Data.Text (Text, unpack)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Lucid (Attribute, Html, ToHtml (toHtml), renderBS)
import Lucid.Base (makeAttribute)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid (HTML)
import qualified XStatic
import qualified XStatic.Htmx as XStatic
import qualified XStatic.Hyperscript as XStatic
import qualified XStatic.Tailwind as XStatic
import Prelude

hxWS, hS, hxSwapOOB :: Text -> Attribute
hxWS = makeAttribute "hx-ws"
hxSwapOOB = makeAttribute "hx-swap-oob"
hS = makeAttribute "_"

type SCHatAPIv1 = "schat" :> "ws" :> WebSocket :<|> "schat" :> Get '[HTML] (Html ())

sChatServer :: SChatS -> Server SCHatAPIv1
sChatServer sChatS = wsChatHandler sChatS :<|> pure sChatHTMLHandler

xStaticFiles :: [XStatic.XStaticFile]
xStaticFiles = [XStatic.htmx, XStatic.tailwind, XStatic.hyperscript]

data EMNotice
  = EMEnter UTCTime Text
  | EMExit UTCTime Text

data Event
  = EMessage Message
  | EMembersRefresh [Text]
  | EMemberNotice EMNotice

data Message = Message
  { date :: UTCTime,
    mLogin :: Text,
    content :: Text
  }
  deriving (Show)

data Client = Client
  { cLogin :: Text,
    conn :: WS.Connection,
    inputQ :: TBQueue Event
  }

data SChatS = SChatS
  { clients :: TVar [Client]
  }

newSChatS :: STM SChatS
newSChatS = do
  clients <- newTVar []
  pure $ SChatS clients

addClient :: Text -> WS.Connection -> SChatS -> STM Client
addClient name conn state = do
  q <- newTBQueue 10
  let newClient = Client name conn q
  modifyTVar (clients state) $ \cls -> do
    cls <> [newClient]
  pure newClient

removeClient :: Text -> SChatS -> STM ()
removeClient cLogin' state = do
  modifyTVar (clients state) $ \cls -> do
    filter (\Client {cLogin} -> not $ cLogin' == cLogin) cls

isClientExists :: Text -> SChatS -> STM Bool
isClientExists cLogin' state = do
  cls <- readTVar $ clients state
  pure $ Prelude.any (\Client {cLogin} -> cLogin == cLogin') cls

wsChatHandler :: SChatS -> WS.Connection -> Handler ()
wsChatHandler state conn = do
  liftIO $ WS.withPingThread conn 5 (pure ()) $ do
    WS.sendTextData conn $ renderBS renderSChat
    ncE <- tryAny $ handleWaitForLogin
    case ncE of
      Right (Just client) -> do
        -- Replace the input box
        WS.sendTextData conn $ renderInputChat client.cLogin
        -- Start handling the ack client
        handleConnection client
      Right Nothing -> do
        putStrLn "Client already exists - closing"
        closeConnection
      Left e -> do
        putStrLn [i|Terminating connection due to #{show e}|]
        closeConnection
  where
    handleConnection (Client myLogin _conn myInputQ) = do
      updateChatMemberOnClients
      date <- getCurrentTime
      dispatchMemberNotice (EMEnter date myLogin)
      concurrently_ handleR handleS
      where
        handleR = do
          hE <- tryAny $ forever handleR'
          case hE of
            Right _ -> pure ()
            Left e -> do
              putStrLn [i|Terminating connection for #{myLogin} due to #{show e}|]
              atomically $ removeClient myLogin state
              updateChatMemberOnClients
              date <- getCurrentTime
              dispatchMemberNotice (EMExit date myLogin)
              closeConnection
          where
            handleR' = do
              wsD <- WS.receiveDataMessage conn
              WS.sendTextData conn $ renderInputChat myLogin
              case extractMessage wsD "chatInputMessage" of
                Just inputMsg -> do
                  now <- getCurrentTime
                  atomically $ do
                    cls <- readTVar state.clients
                    forM_ cls $ \c -> do
                      writeTBQueue c.inputQ $ EMessage (Message now myLogin inputMsg)
                Nothing -> pure ()
        handleS = forever handleS'
          where
            handleS' = do
              event <- atomically $ readTBQueue myInputQ
              hE <- tryAny $ WS.sendTextData conn $ renderBS $ case event of
                EMessage msg -> renderMessage msg
                EMembersRefresh logins -> renderMembersRefresh logins
                EMemberNotice e -> renderMemberNotice e
              case hE of
                Right _ -> pure ()
                Left e -> putStrLn [i|"Unable to send a payload to client #{myLogin} due to #{show e}"|]
            renderMessage :: Message -> Html ()
            renderMessage msg = do
              div_ [id_ "chatroom-content", hxSwapOOB "afterbegin"] $ do
                div_ [id_ "chatroom-message"] $ do
                  span_ [id_ "chatroom-message-date", class_ "pr-2"] . toHtml $ formatDate (msg.date)
                  span_ [id_ "chatroom-message-login", class_ "pr-2"] . toHtml $ unpack (msg.mLogin)
                  span_ [id_ "chatroom-message-content"] . toHtml $ unpack (msg.content)
            renderMembersRefresh :: [Text] -> Html ()
            renderMembersRefresh logins = do
              div_ [id_ "chatroom-members", hxSwapOOB "innerHTML"] $ do
                mapM_ (\mlogin -> div_ [] $ toHtml mlogin) logins
            renderMemberNotice :: EMNotice -> Html ()
            renderMemberNotice emn = do
              div_ [id_ "chatroom-notices", hxSwapOOB "afterbegin"] $ do
                case emn of
                  EMEnter date uLogin -> div_ [] $ [i|#{formatDate date} - #{uLogin} entered the channel|]
                  EMExit date uLogin -> div_ [] $ [i|#{formatDate date} - #{uLogin} exited the channel|]

        dispatchMemberNotice :: EMNotice -> IO ()
        dispatchMemberNotice n = atomically $ do
          cls <- readTVar state.clients
          forM_ cls $ \c -> do
            writeTBQueue c.inputQ $ EMemberNotice n

        updateChatMemberOnClients = atomically $ do
          cls <- readTVar state.clients
          forM_ cls $ \c -> do
            writeTBQueue c.inputQ $ EMembersRefresh (map cLogin cls)

    formatDate = formatTime defaultTimeLocale "%T"

    handleWaitForLogin = do
      login <- waitForLogin
      putStrLn $ "Receiving connection: " <> show login
      atomically $ do
        exists <- isClientExists login state
        case exists of
          False -> do
            newClient <- addClient login conn state
            pure $ Just newClient
          True ->
            pure Nothing
      where
        waitForLogin = do
          wsD <- WS.receiveDataMessage conn
          case extractMessage wsD "chatInputName" of
            Just login -> pure login
            Nothing -> waitForLogin

    closeConnection = do
      WS.sendClose conn ("Bye" :: Text)
      void $ WS.receiveDataMessage conn

    renderInputChat login = renderBS . chatInput $ Just login

    extractMessage dataMessage keyName =
      case dataMessage of
        WS.Text bs _ -> do
          case bs ^? key keyName of
            Just (String m) -> Just m
            _ -> Nothing
        _other -> Nothing

    renderSChat :: Html ()
    renderSChat = do
      div_ [id_ "schat", class_ "h-auto"] $ do
        div_ [class_ "bg-purple-100 border-4 border-purple-300 w-full h-full"] $ do
          title
          chatInput Nothing
          chatDisplay
          chatNotices
      where
        title = p_ [class_ "mb-2 pb-1 bg-purple-300 text-xl"] "Simple WebSocket Chat"
        chatDisplay = do
          div_ [id_ "chatroom", class_ "flex flex-row space-x-2 mx-2 my-2 h-96"] $ do
            roomChat
            roomMembers
          where
            roomChat = do
              div_ [id_ "chatroom-chat", class_ "flex-auto w-3/4 h-full"] $ do
                div_
                  [ id_ "chatroom-content",
                    class_ "overflow-auto border-2 border-purple-200 h-full max-h-full"
                  ]
                  ""
            roomMembers = do
              div_
                [ id_ "chatroom-members",
                  class_ "overflow-auto border-2 border-purple-200 flex-auto w-1/4 h-full max-h-full"
                ]
                ""

    chatInput :: Maybe Text -> Html ()
    chatInput loginM = do
      let inputFieldName = if isJust loginM then "chatInputMessage" else "chatInputName"
      form_ [hxWS "send:submit", id_ "chatroom-input", class_ "mx-2 bg-purple-200 rounded-lg"] $ do
        span_ $ do
          maybe (span_ [] "") (\login -> span_ [class_ "pl-1 pr-2"] $ toHtml login) loginM
          input_
            [ type_ "text",
              class_ "text-sm rounded-lg bg-purple-50 border border-purple-300 focus:border-purple-400",
              name_ inputFieldName,
              id_ "chatroom-input-field",
              placeholder_ "Type a message"
            ]
        script_ "htmx.find('#chatroom-input-field').focus()"

    chatNotices :: Html ()
    chatNotices = do
      div_
        [ id_ "chatroom-notices",
          class_ "overflow-auto mb-2 mx-2 border-2 border-purple-200 h-16 max-h-full"
        ]
        $ ""

sChatHTMLHandler :: Html ()
sChatHTMLHandler = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Simple WebSocket Chat "
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts xStaticFiles
      script_ [iii||]
    body_ $ do
      div_ [class_ "container mx-auto", hxWS "connect:/schat/ws"] $
        div_ [id_ "schat"] ""
