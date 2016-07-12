{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromJust)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger.LogEntries
  ( Config (..)
  , fromString
  , logEntriesLogger
  )
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = requestLogger $ serve api server

requestLogger :: Middleware
requestLogger =
  let token = fromJust . fromString $ "00000000-0000-0000-0000-000000000000"
      logentriesConfig = Config "data.logentries.com" 80 token
  in logEntriesLogger logentriesConfig

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
