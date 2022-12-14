{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cli (main) where

import Chat
import Control.Concurrent.STM (atomically)
import Control.Lens ((^?))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key)
import Data.String.Interpolate (i, iii)
import Data.Text
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
    :<|> xstaticServant Cli.xStaticFiles
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
      xstaticScripts Cli.xStaticFiles
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
        div_ [Cli.hxWS "connect:/ws", Cli.hS "on htmx:oobAfterSwap call #echoInput.reset()"] $ do
          div_ [id_ "echoroom", class_ "border-2"] $ do
            div_ [id_ "echoroom-content"] "chat room placeholder"
          -- name and id attribute are sent in the payload as HX-Trigger-name and HX-Trigger
          form_ [Cli.hxWS "send:submit", name_ "echoInputName", id_ "echoInput"] $ do
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
            div_ [id_ "echoroom-content", Cli.hxSwapOOB "beforeend"] $ do
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

demoApp :: SChatS -> Wai.Application
demoApp sChatS = serve (Proxy @APIv1) $ demoServer sChatS

runServer :: IO ()
runServer = do
  sChatS <- atomically newSChatS
  Warp.run 8091 $ demoApp sChatS

main :: IO ()
main = runServer
