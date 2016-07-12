{- |
  Log message formatting functions
-}
module Network.Wai.Middleware.RequestLogger.Internal.Formatting
  ( buildLogMessage
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Monoid ((<>))
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import System.Log.FastLogger (LogStr, fromLogStr, toLogStr)

{- |
  Adds the user's account token to the log message
-}
buildLogMessage :: UUID -> LogStr -> String
buildLogMessage token logStr =
  (BS8.unpack . fromLogStr $ addToken token logStr) ++ "\n"

addToken :: UUID -> LogStr -> LogStr
addToken token logStr =
  toLogStr $ (BS8.pack $ UUID.toString token) <> " " <> (fromLogStr logStr)
