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

data Message = Message
  { date :: UTCTime,
    mLogin :: Text,
    content :: Text
  }
  deriving (Show)

data Client = Client
  { cLogin :: Text,
    conn :: WS.Connection,
    inputQ :: TBQueue Message
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
    ncE <- tryAny $ handleNewConnection
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
    handleConnection (Client login _conn myInputQ) = do
      concurrently_ handleR handleS
      where
        handleR = do
          hE <- tryAny $ forever handleR'
          case hE of
            Right _ -> pure ()
            Left e -> do
              putStrLn [i|Terminating connection for #{login} due to #{show e}|]
              atomically $ removeClient login state
              closeConnection
          where
            handleR' = do
              wsD <- WS.receiveDataMessage conn
              case extractMessage wsD "chatInputMessage" of
                Just inputMsg -> do
                  now <- getCurrentTime
                  let msg = Message now login inputMsg
                  atomically $ do
                    cls <- readTVar state.clients
                    forM_ cls $ \c -> do
                      writeTBQueue c.inputQ msg
                Nothing -> pure ()
        handleS = forever handleS'
          where
            handleS' = do
              msg <- atomically $ readTBQueue myInputQ
              hE <- tryAny $ WS.sendTextData conn $ renderBS $ do
                div_ [id_ "chatroom-content", hxSwapOOB "afterbegin"] $ do
                  div_ [id_ "chatroom-message"] $ do
                    span_ [id_ "chatroom-message-date", class_ "pr-2"] . toHtml $ formatTime defaultTimeLocale "%T" (msg.date)
                    span_ [id_ "chatroom-message-login", class_ "pr-2"] . toHtml $ unpack (msg.mLogin)
                    span_ [id_ "chatroom-message-content"] . toHtml $ unpack (msg.content)
              case hE of
                Right _ -> pure ()
                Left e -> putStrLn [i|"Unable to send a payload to client #{login} due to #{show e}"|]

    handleNewConnection = do
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
          case extractMessage wsD "chatNameMessage" of
            Just login -> pure login
            Nothing -> waitForLogin

    closeConnection = do
      WS.sendClose conn ("Bye" :: Text)
      void $ WS.receiveDataMessage conn

    renderInputChat login = do
      renderBS $ do
        form_ [hxWS "send:submit", id_ "chatroom-input", class_ "bg-purple-200"] $ do
          span_ $ do
            span_ [class_ "pr-2"] $ toHtml login
            input_
              [ type_ "text",
                name_ "chatInputMessage",
                placeholder_ "Type a message"
              ]

    extractMessage dataMessage keyName =
      case dataMessage of
        WS.Text bs _ -> do
          case bs ^? key keyName of
            Just (String m) -> Just m
            _ -> Nothing
        _other -> Nothing

sChatHTMLHandler :: Html ()
sChatHTMLHandler = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Simple WebSocket Chat "
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts xStaticFiles
      script_ [iii||]
    body_ $ do
      div_ [class_ "container mx-auto h-64"] $ do
        div_ [class_ "bg-purple-100 border-4 border-purple-300 w-full h-full"] $ do
          p_ [class_ "mb-2 bg-purple-200 text-xl"] "Simple WebSocket Chat"
          div_ [class_ "h-32", hxWS "connect:/schat/ws", hS "on htmx:oobAfterSwap call #chatInput.reset()"] $ do
            div_ [id_ "chatroom", class_ "flex flex-row space-x-2 mx-2 my-2 h-full"] $ do
              div_ [id_ "chatroom-chat", class_ "flex-auto w-2/3 h-full"] $ do
                div_ [id_ "chatroom-content", class_ "overflow-auto border-2 border-purple-200 h-full max-h-full"] ""
              div_ [id_ "chatroom-members", class_ "border-2 border-purple-200 flex-auto w-1/3 h-full max-h-full"] ""
            form_ [id_ "chatroom-input", class_ "bg-purple-200", hxWS "send:submit"] $ do
              input_
                [ type_ "text",
                  name_ "chatNameMessage",
                  placeholder_ "Type your name"
                ]
