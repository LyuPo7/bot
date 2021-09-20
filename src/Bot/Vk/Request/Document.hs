{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Request.Document where

import qualified System.Directory as Dir

import Bot.Vk.Request.DocumentSpec (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.DocumentImpl as DocImpl

withHandleIO :: Logger.Handle IO -> ParserSpec.Handle IO -> ReqSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger parserh reqh f = do
  let handle = Handle {
    hLogger = logger,
    hReq = reqh,
    hParser = parserh,

    getTemporaryDirectory = Dir.getTemporaryDirectory,
    saveUploadedDoc = DocImpl.saveUploadedDoc reqh,
    downloadFile = DocImpl.downloadFile,
    getUploadedServer = DocImpl.getUploadedServer reqh,
    uploadFile = DocImpl.uploadFile
  }
  f handle