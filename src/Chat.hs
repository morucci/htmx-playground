{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Chat where

import Control.Concurrent.STM
import Control.Lens ((^?))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key)
import Data.String.Interpolate (iii)
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
    conn :: WS.Connection
  }

data SChatS = SChatS
  { clients :: TVar [Client],
    queue :: TBQueue Message
  }

newSChatS :: STM SChatS
newSChatS = do
  rq <- newTBQueue 10
  clients <- newTVar []
  pure $ SChatS clients rq

addClient :: Text -> WS.Connection -> SChatS -> STM ()
addClient name conn state = do
  modifyTVar (clients state) $ \cls -> do
    cls <> [Client name conn]

isClientExists :: Text -> SChatS -> STM Bool
isClientExists name' state = do
  cls <- readTVar $ clients state
  pure $ Prelude.any (\Client {name} -> name == name') cls

wsChatHandler :: SChatS -> WS.Connection -> Handler ()
wsChatHandler state conn = do
  liftIO $ WS.withPingThread conn 5 (pure ()) $ do
    nameM <- handleNewConnection
    case nameM of
      Just name -> do
        -- Replace the input box
        WS.sendTextData conn $ renderInputChat name
        -- Start handling the ack client
        handleConnection name
      Nothing -> do
        putStrLn "Client already exists - closing"
        pure () -- losely close the connection
  where
    handleConnection name = handle
      where
        handle = do
          -- Wait for an input message
          wsD <- liftIO $ WS.receiveDataMessage conn
          now <- getCurrentTime
          case extractMessage wsD of
            Just msg -> do
              putStrLn $ "Received: " <> show msg
              atomically $ do
                writeTBQueue (queue state) $ Message now name msg
              handle
            Nothing -> handle

    handleNewConnection = do
      name <- waitForName
      putStrLn $ "Receiving connection: " <> show name
      atomically $ do
        exists <- isClientExists name state
        case exists of
          False -> do
            addClient name conn state
            pure $ Just name
          True ->
            pure Nothing
      where
        waitForName = do
          wsD <- liftIO $ WS.receiveDataMessage conn
          case extractLoginName wsD of
            Just name -> pure name
            Nothing -> waitForName
        extractLoginName dataM =
          case dataM of
            WS.Text bs _ -> do
              case bs ^? key "chatNameMessage" of
                Just (String m) -> Just m
                _ -> Nothing
            _other -> Nothing

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

    extractMessage :: WS.DataMessage -> Maybe Text
    extractMessage dataMessage =
      case dataMessage of
        WS.Text bs _ -> do
          case bs ^? key "chatInputMessage" of
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
            div_ [id_ "chatroom-content"] "chat room placeholder"
          form_ [hxWS "send:submit", name_ "chatName", id_ "Input"] $ do
            input_
              [ type_ "text",
                name_ "chatNameMessage",
                placeholder_ "Type your name"
              ]

dispatcher :: SChatS -> IO ()
dispatcher SChatS {..} = forever $ do
  (m, c) <- atomically $ do
    msg <- readTBQueue queue
    cls <- readTVar clients
    pure (msg, cls)
  mapM_ (sendToClient m) c
  where
    sendToClient msg Client {..} = do
      WS.sendTextData conn $ renderBS $ do
        div_ [id_ "chatroom-content", hxSwapOOB "beforeend"] $ do
          div_ $ messageToHtml msg
      where
        messageToHtml Message {..} =
          toHtml $
            (show date) <> show login <> show content

-- WSChat End
