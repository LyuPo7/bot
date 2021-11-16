module Bot.System.SystemImplementation where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified System.Directory as Dir
import Data.Text (Text)
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.System.System as BotSystem
import qualified Bot.Settings as Settings
import qualified System.IO as SIO

withHandleIO :: Logger.Handle IO -> Settings.Config ->
               (BotSystem.Handle IO -> IO a) -> IO a
withHandleIO logger config f = do
  let handle = BotSystem.Handle {
    BotSystem.hLogger = logger,
    BotSystem.cSet = config,

    BotSystem.readFile = Prelude.readFile,
    BotSystem.getTemporaryDirectory = Dir.getTemporaryDirectory,
    BotSystem.downloadFile = downloadFile logger,
    BotSystem.uploadFile = uploadFile logger
  }
  f handle

downloadFile :: Logger.Handle IO -> Text -> FilePath -> IO ()
downloadFile logH link fileName = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ T.unpack link
  response <- httpLbs request manager
  file <- SIO.openBinaryFile fileName SIO.WriteMode
  SIO.hPutStr file (L8.unpack $ responseBody response)
  SIO.hClose file
  Logger.logInfo logH "Doc was downloaded."

uploadFile :: Logger.Handle IO -> Text -> FilePath -> IO B.ByteString
uploadFile logH link fileName = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest (T.unpack link)
  response <- flip httpLbs manager =<< formDataBody form req
  Logger.logInfo logH "Doc was uploaded."
  return $ responseBody response
  where form = [ partFileSource "file" fileName ]