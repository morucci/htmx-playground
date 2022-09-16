{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cli (main) where

import Data.Text
import GHC.Generics (Generic)
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
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

hxGet, hxPost, hxTrigger, hxTarget, hxSwap :: Text -> Attribute
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxSwap = makeAttribute "hx-swap"

data MessageForm = MessageForm
  { message :: Text
  }
  deriving (Generic)

type APIv1 =
  Get '[HTML] (Html ())
    :<|> "xstatic" :> Raw
    :<|> "ws" :> WebSocket
    :<|> "messages" :> Get '[HTML] (Html ())
    :<|> "messagesPost" :> ReqBody '[FormUrlEncoded] MessageForm :> Post '[HTML] (Html ())

demoServer :: Server APIv1
demoServer =
  pure indexHtml
    :<|> xstaticServant [XStatic.htmx, Xstatic.tailwind]
    :<|> wsHandler
    :<|> pure messagesHandler
    :<|> messagesPostHandler

wsHandler :: WS.Connection -> Handler ()
wsHandler _c = pure ()

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
        span_ [class_ "border-2 pr-2", hxGet "/messages", hxTrigger "click", hxSwap "afterend"] $ do
          "Click here to display the GET result after the content"

      div_ [class_ "pb-2"] $ do
        form_ [hxPost "/messagesPost", hxSwap "afterend"] $ do
          input_ [id_ "messageForm", name_ "message", type_ "text"]
          button_ [type_ "submit", class_ "border-2 bg-blue-500 text-white"] "Submit"

messagesHandler :: Html ()
messagesHandler = do
  span_ "Here is a message from the /messages endpoint"

messagesPostHandler :: MessageForm -> Handler (Html ())
messagesPostHandler (MessageForm m) = pure $ do
  p_ $ toHtml m

demoApp :: Wai.Application
demoApp = serve (Proxy @APIv1) $ demoServer

runServer :: IO ()
runServer = Warp.run 8090 demoApp

main :: IO ()
main = runServer
