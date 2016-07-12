{- |
  Entrypoint module for the package
-}
module Network.Wai.Middleware.RequestLogger.LogEntries
  ( Config (..)
  , logEntriesLogger

  -- UUID export to make configuration a bit easier
  , UUID
  , UUID.fromString
  , UUID.nil
  ) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger
  ( RequestLoggerSettings (..)
  , OutputFormat (..)
  , Destination (..)
  , IPAddrSource (..)
  , mkRequestLogger)
import Network.Wai.Middleware.RequestLogger.Internal.Formatting (buildLogMessage)
import qualified Data.ByteString.Char8 as BS8
import Data.Default (def)
import Data.Monoid ((<>))
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Network.Socket
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger (LogStr, fromLogStr, toLogStr)

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import qualified Control.Concurrent.STM as STM

{- |
  Account configuration
-}
data Config = Config
  { hostname :: String
  , port     :: Int
  , token    :: UUID
  } deriving (Show)

{- |
  Function to create the LogEntries Middleware
-}
logEntriesLogger :: Config -> Middleware
logEntriesLogger config = unsafePerformIO $ do
  logChan <- STM.newTChanIO
  forkIO (logRequests config logChan)
   >> mkRequestLogger (logEntriesSettings logChan)

logEntriesSettings :: STM.TChan LogStr -> RequestLoggerSettings
logEntriesSettings logChan = def
  { outputFormat = Apache FromSocket
  , destination  = Callback (addLogMessage logChan)
  }

addLogMessage :: STM.TChan LogStr -> LogStr -> IO ()
addLogMessage logChan logStr = do
  STM.atomically $ STM.writeTChan logChan logStr
  return ()

sendLogMessage :: Config -> STM.TChan LogStr -> Socket -> IO ()
sendLogMessage config@Config{..} logChan sock = do
  logStr <- STM.atomically $ STM.readTChan logChan
  _      <- send sock $ buildLogMessage token logStr
  sendLogMessage config logChan sock -- recur forever

logRequests :: Config -> STM.TChan LogStr -> IO ()
logRequests config@Config{..} logChan = do
  addrs <- getAddrInfo Nothing (Just hostname) (Just . show $ port)
  case addrs of
    [] -> return ()
    (serverAddr:_) ->
      bracket
        (openSocket serverAddr)
        sClose
        (sendLogMessage config logChan)

openSocket :: AddrInfo -> IO Socket
openSocket AddrInfo{..} = do
  sock <- socket addrFamily Stream defaultProtocol
  connect sock addrAddress
  return sock
