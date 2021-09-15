{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.Document where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics ()
import Data.Aeson ()
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status ()
import Network.HTTP.Client.MultipartFormData
import Control.Monad ()
import qualified System.IO as SIO
import System.Directory

import Bot.Vk.Request.RequestsSpec (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Request.Requests as Req
import Bot.Vk.Parser.Data
import Bot.Vk.Request.Data

getUploadedServer :: Handle IO -> Integer -> Text -> IO B.ByteString
getUploadedServer handle peerId fileType = do
  let config = configReq handle
      token = Settings.botToken config
      query = getLink peerId fileType Settings.vkVersion token
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable query
  Req.makeRequest handle getMessagesUploadServer queryOptions

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadFile :: Text -> FilePath -> IO ()
downloadFile link fileName = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ T.unpack link
  response <- httpLbs request manager
  file <- SIO.openBinaryFile fileName SIO.WriteMode
  SIO.hPutStr file (L8.unpack $ responseBody response)
  SIO.hClose file

uploadFile :: Text -> FilePath -> IO B.ByteString
uploadFile link fileName = do
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
  manager <- newManager tlsManagerSettings
  req <- parseRequest (T.unpack link)
  response <- flip httpLbs manager =<< formDataBody form req
  return $ responseBody response
  where form = [ partFileSource "file" fileName ]

saveUploadedDoc :: Handle IO -> Text -> IO B.ByteString
saveUploadedDoc handle file = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      link = saveNewDoc file token Settings.vkVersion
  Logger.logInfo logh "Doc was saved."
  let queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable link
  Req.makeRequest handle saveDoc queryOptions

-- work with Doc Attachment
updateDoc :: Handle IO -> Document -> IO Document
updateDoc handle doc = do
  let logh = hLogger handle
      parseh = hParser handle
      -- extract url from file
      link = document_url doc
      title = document_title doc
      userId = document_ownerId doc
  -- create temp directory
  tempDir <- getTemporaryDirectory
  (tempFileName, _) <- SIO.openTempFile tempDir (T.unpack title)
  -- download file
  downloadFile link tempFileName
  Logger.logInfo logh "File was downloaded"
  -- get server for upload file
  serverUp <- getUploadedServer handle userId (T.pack "doc")
  urlResp <- Parser.parseUploadUrl parseh serverUp
  let url = maybe (T.pack "") upUrl_uploadUrl (upUrlResponse_response urlResp)
  -- upload file
  fileUp <- uploadFile url tempFileName
  fileResp <- Parser.parseUploadFile parseh fileUp
  let file = fromMaybe (T.pack "") $ upFileResponse_file fileResp
  Logger.logInfo logh "File was uploaded"
  -- save file
  objUp <- saveUploadedDoc handle file
  obj <- Parser.parseUploadObject parseh objUp
  -- remove tempFile
  --removeDirectoryRecursive tempDir
  case upObjResponse_response obj of
    [x] -> do
      let urlNew = upObj_url x
          idNew = upObj_id x
          idOwnerNew = upObj_ownerId x
      Logger.logInfo logh "Doc changed"
      return doc {
        document_id = idNew,
        document_ownerId = idOwnerNew,
        document_url = urlNew
      }
    _ -> do
      Logger.logWarning logh "No uploaded objects" -- Maybe only one uploaded object
      return doc