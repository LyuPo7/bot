module TestBot.Handlers where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.Internal as HTTPInternal

import qualified Bot.Logger.Logger as Logger

logH :: Logger.Handle Maybe
logH = Logger.Handle {
  Logger.log = \_ _ -> return (),
  Logger.hConfig = Logger.Config {Logger.cVerbosity = Nothing}
}

resp :: B.ByteString -> HTTPInternal.Response B.ByteString
resp text = HTTPInternal.Response {
  HTTPInternal.responseStatus = status,
  HTTPInternal.responseBody = text,
  HTTPInternal.responseVersion = undefined,
  HTTPInternal.responseHeaders = undefined,
  HTTPInternal.responseCookieJar = undefined,
  HTTPInternal.responseClose' = undefined
}

status :: HTTPTypes.Status
status = HTTPTypes.Status {
  HTTPTypes.statusCode = 200,
  HTTPTypes.statusMessage = ""
}