module Network.Wai.Middleware.RequestLogger.Internal.FormattingSpec where

import qualified Data.UUID.Types as UUID
import Network.Wai.Middleware.RequestLogger.Internal.Formatting
import System.Log.FastLogger (toLogStr)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildLogMessage" $ do
    it "prepends the UUID to the log message and appends a newline char" $
      let uuid = UUID.nil
          logMsg = toLogStr ("This is my log message"::String)
          expectedLogMsg = "00000000-0000-0000-0000-000000000000 This is my log message\n"
      in buildLogMessage uuid logMsg `shouldBe` expectedLogMsg

