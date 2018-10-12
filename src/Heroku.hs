{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Heroku (serveHerokuPingServer) where

import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import System.Environment (getEnv)

type Root = "/" :> Get '[JSON] Text

rootController :: Server Root
rootController = return "OK"

rootAPI :: Proxy Root
rootAPI = Proxy

rootApp :: Application
rootApp = serve rootAPI rootController

serveHerokuPingServer :: IO ()
serveHerokuPingServer = do
  port <- read <$> getEnv "PORT"
  run port rootApp
