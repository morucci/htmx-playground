{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cli (main) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
  ( STM,
    TBQueue,
    TVar,
    atomically,
    modifyTVar,
    newTBQueue,
    newTVar,
    readTBQueue,
    readTVar,
    writeTBQueue,
  )
import Control.Lens ((^?))
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key)
import Data.String.Interpolate (i, iii)
import Data.Text
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.WebSockets (receiveDataMessage, withPingThread)
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import Servant.XStatic
import Web.FormUrlEncoded (FromForm)
import qualified XStatic
import qualified XStatic.Htmx as XStatic
import qualified XStatic.Hyperscript as XStatic
import qualified XStatic.Tailwind as XStatic
import Prelude

-- Use https://hackage.haskell.org/package/lucid-htmx
hxGet, hxPost, hxTrigger, hxTarget, hxSwap, hxVals, hxWS, hxSwapOOB, hS :: Text -> Attribute
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxSwap = makeAttribute "hx-swap"
hxVals = makeAttribute "hx-vals"
hxWS = makeAttribute "hx-ws"
hxSwapOOB = makeAttribute "hx-swap-oob"
hS = makeAttribute "_"

data MessageForm = MessageForm
  { message :: Text
  }
  deriving (Generic)

type SearchForm = MessageForm

type APIv1 =
  Get '[HTML] (Html ())
    :<|> "xstatic" :> Raw
    :<|> "ws" :> WebSocket
    :<|> "messages" :> Get '[HTML] (Html ())
    :<|> "messagesBis" :> Header "HX-Target" Text :> Get '[HTML] (Html ())
    :<|> "messagesPost" :> ReqBody '[FormUrlEncoded] MessageForm :> Post '[HTML] (Html ())
    :<|> "searchUsers" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] (Html ())
    :<|> SCHatAPIv1

xStaticFiles :: [XStatic.XStaticFile]
xStaticFiles = [XStatic.htmx, XStatic.tailwind, XStatic.hyperscript]

demoServer :: SChatS -> Server APIv1
demoServer sChatS =
  pure indexHtml
    :<|> xstaticServant xStaticFiles
    :<|> wsHandler
    :<|> pure messagesHandler
    :<|> messagesBisHandler
    :<|> messagesPostHandler
    :<|> searchUsersHandler
    :<|> sChatServer sChatS

instance FromForm MessageForm

indexHtml :: Html ()
indexHtml = do
  doctypehtml_ $ do
    head_ $ do
      title_ "HTMX playground page"
      xstaticScripts xStaticFiles
      -- https://htmx.org/docs/#events
      script_
        [iii|
        console.log('Hello HTMX');
        htmx.logger = function(elt, event, data) {
          if(console) {
            console.log(event, elt, data);
          }
        }
      |]
    body_ $ do
      div_ [class_ "pb-2"] $ do
        h1_ [class_ "text-lg"] "Welcome in htmx demo"

      div_ [class_ "pb-2"] $ do
        div_ [class_ "border-2", hxGet "/messages"] $ do
          "Click here to replace the content with the GET result"

      div_ [class_ "pb-2"] $ do
        span_ [class_ "border-2 pr-2", hxGet "/messages", hxTrigger "click", hxTarget "#ph1"] $ do
          "Click here to display the GET result in the placeholder"
        span_ [id_ "ph1"] "placeholder"

      div_ [class_ "pb-2"] $ do
        span_
          [ id_ "messageSender", -- is sent in HX-trigger request headers
            hxVals "'{\"foo\": \"bar\"}'", -- should be sent as request parameters
            class_ "border-2 pr-2",
            hxGet "/messagesBis",
            hxTrigger "click",
            hxSwap "afterend"
          ]
          $ do
            "Click here to display the GET result after the content"

      div_ [class_ "pb-2"] $ do
        p_ "Echo room via post"
        form_ [hxPost "/messagesPost", hxSwap "afterend"] $ do
          input_ [id_ "messageForm", name_ "message", type_ "text"]
          button_
            [ type_ "submit",
              class_ "border-2 bg-blue-500 text-white"
            ]
            "Submit"

      div_ [class_ "pb-2"] $ do
        p_ "Active search"
        form_ [] $ do
          input_
            [ id_ "searchForm",
              name_ "message",
              placeholder_ "Start to type",
              type_ "search",
              hxPost "/searchUsers",
              hxTarget "#ph2",
              hxTrigger "keyup[target.value.length > 1] changed delay:500ms, search"
            ]
        span_ [id_ "ph2"] "Results placeholder"

      div_ [class_ "pb-2"] $ do
        p_ "Echo room via websocket"
        div_ [hxWS "connect:/ws", hS "on htmx:oobAfterSwap call #echoInput.reset()"] $ do
          div_ [id_ "echoroom", class_ "border-2"] $ do
            div_ [id_ "echoroom-content"] "chat room placeholder"
          -- name and id attribute are sent in the payload as HX-Trigger-name and HX-Trigger
          form_ [hxWS "send:submit", name_ "echoInputName", id_ "echoInput"] $ do
            input_
              [ type_ "text",
                name_ "echoInputMessage",
                placeholder_ "Type a message"
              ]

messagesHandler :: Html ()
messagesHandler = do
  span_ "Here is a message from the /messages endpoint"

messagesBisHandler :: Maybe Text -> Handler (Html ())
messagesBisHandler hM = pure $ do
  span_ . toHtml @Text $ [i|"Message from /messageBis (header HX-Trigger sent: #{show hM})"|]

messagesPostHandler :: MessageForm -> Handler (Html ())
messagesPostHandler (MessageForm m) = pure $ do
  p_ $ toHtml m

searchUsersHandler :: SearchForm -> Handler (Html ())
searchUsersHandler (MessageForm search) = pure $ do
  let users = ["John Doe", "Jane Doe", "Sarah Connor", "Walter White"]
      found = Prelude.filter (isInfixOf search) users
  when (Prelude.null found) $ p_ "No result"
  mapM_ render found
  where
    render :: Text -> Html ()
    render item = p_ $ toHtml item

wsHandler :: WS.Connection -> Handler ()
wsHandler conn = do
  liftIO $ withPingThread conn 5 (pure ()) $ handleConnection
  where
    handleConnection = do
      wsD <- liftIO $ receiveDataMessage conn
      case extractMessage wsD of
        Just msg -> do
          liftIO . putStrLn $ "Received: " <> show msg
          WS.sendTextData conn $ renderBS $ do
            div_ [id_ "echoroom-content", hxSwapOOB "beforeend"] $ do
              div_ $ toHtml msg
          handleConnection
        Nothing -> handleConnection
    extractMessage :: WS.DataMessage -> Maybe Text
    extractMessage dataMessage =
      case dataMessage of
        WS.Text bs _ -> do
          case bs ^? key "echoInputMessage" of
            Just (String m) -> Just m
            _ -> Nothing
        _other -> Nothing

-- WSChat Begin

type SCHatAPIv1 = "schat" :> "ws" :> WebSocket :<|> "schat" :> Get '[HTML] (Html ())

sChatServer :: SChatS -> Server SCHatAPIv1
sChatServer sChatS = wsChatHandler sChatS :<|> pure sChatHTMLHandler

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
  liftIO $ withPingThread conn 5 (pure ()) $ do
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
          wsD <- liftIO $ receiveDataMessage conn
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
          wsD <- liftIO $ receiveDataMessage conn
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

demoApp :: SChatS -> Wai.Application
demoApp sChatS = serve (Proxy @APIv1) $ demoServer sChatS

runServer :: IO ()
runServer = do
  sChatS <- atomically newSChatS
  concurrently_
    (Warp.run 8091 $ demoApp sChatS)
    (dispatcher sChatS)

main :: IO ()
main = runServer
