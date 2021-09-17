{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

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
    saveUploadedDoc = \text -> DocImpl.saveUploadedDoc reqh text,
    downloadFile = \text fp -> DocImpl.downloadFile text fp,
    getUploadedServer = \int text -> DocImpl.getUploadedServer reqh int text,
    uploadFile = \text fp -> DocImpl.uploadFile text fp
  }
  f handle