{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Request.DocumentSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import System.FilePath.Posix ((</>))

import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Logger as Logger
import Bot.Vk.Parser.Data

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    hReq :: ReqSpec.Handle m,
    hParser :: ParserSpec.Handle m,

    getTemporaryDirectory :: m FilePath,
    saveUploadedDoc :: Text -> m B.ByteString,
    downloadFile :: Text -> FilePath -> m (),
    getUploadedServer :: Integer -> Text -> m B.ByteString,
    uploadFile :: Text -> FilePath -> m B.ByteString
}

-- work with Doc Attachment
updateDoc :: Monad m => Handle m -> Document -> m Document
updateDoc handle doc = do
  let logh = hLogger handle
      parseh = hParser handle
      -- extract url from file
      link = document_url doc
      title = document_title doc
      userId = document_ownerId doc
  -- create temp directory
  tempDir <- getTemporaryDirectory handle
  let fileName = tempDir </> T.unpack title
  -- download file
  downloadFile handle link fileName
  Logger.logInfo logh "File was downloaded"
  -- get server for upload file
  serverUp <- getUploadedServer handle userId (T.pack "doc")
  urlResp <- Parser.parseUploadUrl parseh serverUp
  let url = maybe (T.pack "") upUrl_uploadUrl (upUrlResponse_response urlResp)
  -- upload file
  fileUp <- uploadFile handle url fileName
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
      Logger.logInfo logh "Doc changed"
      return doc {
        document_id = upObj_id x,
        document_ownerId = upObj_ownerId x,
        document_url = upObj_url x
      }
    _ -> do
       -- Maybe only one uploaded object
      Logger.logWarning logh "No uploaded objects"
      return doc