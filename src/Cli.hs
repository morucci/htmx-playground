{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cli (main) where

import Control.Monad.IO.Class (MonadIO)
import Lucid
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Servant.XStatic
import qualified XStatic.Htmx as XStatic
import Prelude

type APIv1 =
  "xstatic" :> Raw
    :<|> "ws" :> WebSocket
    :<|> "app" :> Get '[HTML] (Html ())

demoServer :: Server APIv1
demoServer =
  xstaticServant [XStatic.htmx]
    :<|> wsServer
    :<|> pure htmlServer

wsServer :: Server WebSocket
wsServer = streamData
  where
    streamData :: MonadIO m => WS.Connection -> m ()
    streamData _c = pure ()

htmlServer :: Html ()
htmlServer = do
  doctypehtml_ $ do
    head_ $ do
      title_ "HTMX playground page"
      xstaticScripts [XStatic.htmx]
    body_ $ do
      p_ "Hello"

demoApp :: Wai.Application
demoApp = serve (Proxy @APIv1) $ demoServer

runServer :: IO ()
runServer = Warp.run 8090 demoApp

main :: IO ()
main = runServer
