{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Chat where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Lens ((^?))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key)
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
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
    login :: Text,
    content :: Text
  }
  deriving (Show)

data Client = Client
  { name :: Text,
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
removeClient name' state = do
  modifyTVar (clients state) $ \cls -> do
    filter (\Client {name} -> not $ name' == name) cls

isClientExists :: Text -> SChatS -> STM Bool
isClientExists name' state = do
  cls <- readTVar $ clients state
  pure $ Prelude.any (\Client {name} -> name == name') cls

wsChatHandler :: SChatS -> WS.Connection -> Handler ()
wsChatHandler state conn = do
  liftIO $ WS.withPingThread conn 5 (pure ()) $ do
    ncE <- tryAny $ handleNewConnection
    case ncE of
      Right (Just client) -> do
        -- Replace the input box
        WS.sendTextData conn $ renderInputChat (name client)
        -- Start handling the ack client
        handleConnection client
      Right Nothing -> do
        putStrLn "Client already exists - closing"
        closeConnection
      Left e -> do
        putStrLn [i|Terminating connection due to #{show e}|]
        closeConnection
  where
    handleConnection (Client name _c myInputQ) = do
      concurrently_ handleR handleS
      where
        handleR = do
          hE <- tryAny handleR'
          case hE of
            Right _ -> pure ()
            Left e -> do
              putStrLn [i|Terminating connection for #{name} due to #{show e}|]
              atomically $ removeClient name state
              closeConnection
          where
            handleR' = do
              wsD <- WS.receiveDataMessage conn
              case extractMessage wsD "chatInputMessage" of
                Just txt -> do
                  now <- getCurrentTime
                  let message = Message now name txt
                  putStrLn [i|Received: #{show message}|]
                  atomically $ do
                    cls <- readTVar $ clients state
                    mapM_ (\c -> writeTBQueue (inputQ c) message) cls
                  -- writeTBQueue (queue state) message
                  handleR'
                Nothing -> handleR'
        handleS = forever handleS'
          where
            handleS' = do
              msg <- atomically $ readTBQueue myInputQ
              hE <- tryAny $ WS.sendTextData conn $ renderBS $ do
                div_ [id_ "chatroom-content", hxSwapOOB "beforeend"] $ do
                  div_ $ messageToHtml msg
              case hE of
                Right _ -> pure ()
                Left e -> putStrLn [i|"Unable to send a payload to client #{name} due to #{show e}"|]
            messageToHtml Message {..} =
              toHtml $
                (show date) <> show login <> show content

    handleNewConnection = do
      name <- waitForName
      putStrLn $ "Receiving connection: " <> show name
      atomically $ do
        exists <- isClientExists name state
        case exists of
          False -> do
            newClient <- addClient name conn state
            pure $ Just newClient
          True ->
            pure Nothing
      where
        waitForName = do
          wsD <- WS.receiveDataMessage conn
          case extractMessage wsD "chatNameMessage" of
            Just name -> pure name
            Nothing -> waitForName

    closeConnection = do
      WS.sendClose conn ("Bye" :: Text)
      void $ WS.receiveDataMessage conn

    renderInputChat name = do
      renderBS $ do
        form_ [hxWS "send:submit", name_ "chatInput", id_ "Input"] $ do
          span_ $ do
            span_ [class_ "pr-2"] $ toHtml name
            input_
              [ type_ "text",
                name_ "chatInputMessage",
                placeholder_ "Type a message"
              ]

    extractMessage :: WS.DataMessage -> Text -> Maybe Text
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
      xstaticScripts xStaticFiles
      script_ [iii||]
    body_ $ do
      div_ [class_ "pb-2"] $ do
        p_ "Simple WebSocket Chat"
        div_ [hxWS "connect:/schat/ws", hS "on htmx:oobAfterSwap call #chatInput.reset()"] $ do
          div_ [id_ "chatroom", class_ "border-2"] $ do
            div_ [id_ "chatroom-content"] ""
          form_ [hxWS "send:submit", name_ "chatName", id_ "Input"] $ do
            input_
              [ type_ "text",
                name_ "chatNameMessage",
                placeholder_ "Type your name"
              ]
