module Bot.Vk.Request.DocumentSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import System.FilePath.Posix ((</>))

import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Logger as Logger
import Bot.Vk.Parser.Objects.Document (Document(..))
import qualified Bot.Vk.Parser.Objects.Document as Doc
import qualified Bot.Vk.Parser.Objects.UploadObject as UpObject
import qualified Bot.Vk.Parser.Objects.UploadUrl as UpUrl
import qualified Bot.Vk.Parser.Objects.UploadObjectResponse as UpObjectResp
import qualified Bot.Vk.Parser.Objects.UploadFileResponse as UpFileResp
import qualified Bot.Vk.Parser.Objects.UploadUrlResponse as UpUrlResp

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
  let logH = hLogger handle
      parseH = hParser handle
      -- extract url from file
      link = Doc.url doc
      docTitle = Doc.title doc
      userId = Doc.owner_id doc
  -- create temp directory
  tempDir <- getTemporaryDirectory handle
  let fileName = tempDir </> T.unpack docTitle
  -- download file
  downloadFile handle link fileName
  Logger.logInfo logH "File was downloaded"
  -- get server for upload file
  serverUp <- getUploadedServer handle userId (T.pack "doc")
  urlResp <- Parser.parseUploadUrl parseH serverUp
  let docUrl = maybe (T.pack "") UpUrl.upload_url (UpUrlResp.response urlResp)
  -- upload file
  fileUp <- uploadFile handle docUrl fileName
  fileResp <- Parser.parseUploadFile parseH fileUp
  case UpFileResp.file fileResp of
    Nothing -> do
      Logger.logError logH "File wasn't uploaded"
      return doc
    Just file -> do
      Logger.logInfo logH "File was uploaded"
      -- save file
      objUp <- saveUploadedDoc handle file
      obj <- Parser.parseUploadObject parseH objUp
      case UpObjectResp.response obj of
        [x] -> do
          Logger.logInfo logH "Doc changed"
          return doc {
            Doc.id = UpObject.id x,
            Doc.owner_id = UpObject.owner_id x,
            Doc.url = UpObject.url x
          }
        _ -> do
          -- Maybe only one uploaded object
          Logger.logWarning logH "No uploaded objects"
          return doc