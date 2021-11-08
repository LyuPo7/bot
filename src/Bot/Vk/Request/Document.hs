module Bot.Vk.Request.Document where

import qualified System.Directory as Dir

import Bot.Vk.Request.DocumentSpec (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.DocumentImpl as DocImpl

withHandleIO :: Logger.Handle IO -> ParserSpec.Handle IO ->
                ReqSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger parserH reqH f = do
  let handle = Handle {
    hLogger = logger,
    hReq = reqH,
    hParser = parserH,

    getTemporaryDirectory = Dir.getTemporaryDirectory,
    saveUploadedDoc = DocImpl.saveUploadedDoc reqH,
    downloadFile = DocImpl.downloadFile,
    getUploadedServer = DocImpl.getUploadedServer reqH,
    uploadFile = DocImpl.uploadFile
  }
  f handle