{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cli (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text
import GHC.Generics (Generic)
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.WebSockets (receive, withPingThread)
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Servant.XStatic
import Web.FormUrlEncoded (FromForm)
import qualified XStatic.Htmx as XStatic
import qualified XStatic.Tailwind as XStatic
import qualified XStatic.Tailwind as Xstatic
import Prelude

hxGet, hxPost, hxTrigger, hxTarget, hxSwap, hxVals, hxWS :: Text -> Attribute
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxSwap = makeAttribute "hx-swap"
hxVals = makeAttribute "hx-vals"
hxWS = makeAttribute "hx-ws"

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

demoServer :: Server APIv1
demoServer =
  pure indexHtml
    :<|> xstaticServant [XStatic.htmx, Xstatic.tailwind]
    :<|> wsHandler
    :<|> pure messagesHandler
    :<|> messagesBisHandler
    :<|> messagesPostHandler
    :<|> searchUsersHandler

instance FromForm MessageForm

indexHtml :: Html ()
indexHtml = do
  doctypehtml_ $ do
    head_ $ do
      title_ "HTMX playground page"
      xstaticScripts [XStatic.htmx, XStatic.tailwind]
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
        form_ [hxPost "/messagesPost", hxSwap "afterend"] $ do
          input_ [id_ "messageForm", name_ "message", type_ "text"]
          button_ [type_ "submit", class_ "border-2 bg-blue-500 text-white"] "Submit"

      div_ [class_ "pb-2"] $ do
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
        span_ [id_ "ph2"] "placeholder"

      div_ [class_ "pb-2"] $ do
        p_ "Chat room via websocket"
        div_ [hxWS "connect:/ws"] $ do
          div_ [id_ "chatroom", class_ "border-2"] "chat room placeholder"
          form_ [hxWS "send:submit"] $ do
            input_
              [ id_ "chatmessageId",
                name_ "chatmessage",
                type_ "text",
                placeholder_ "Type a chat message"
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
  liftIO $ withPingThread conn 5 (pure ()) $ do
    wsD <- liftIO $ receive conn
    liftIO $ putStrLn $ show wsD
    pure ()

demoApp :: Wai.Application
demoApp = serve (Proxy @APIv1) $ demoServer

runServer :: IO ()
runServer = Warp.run 8090 demoApp

main :: IO ()
main = runServer
