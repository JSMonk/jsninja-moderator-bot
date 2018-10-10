{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Heroku
  ( runHerokuPing
  ) where

import Data.Text (Text)
import Servant
import Servant.API
import Network.Wai.Handler.Warp

type API = "/" :> Get '[JSON] Text

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = return "OK"

runHerokuPing :: String -> IO ()
runHerokuPing port = run (read port) app
