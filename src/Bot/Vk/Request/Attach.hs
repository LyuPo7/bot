{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.Attach where

import Bot.Vk.Request.AttachSpec (Handle(..))
import qualified Bot.Vk.Request.Document as Doc
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Logger as Logger

withHandleIO :: Logger.Handle IO -> ParserSpec.Handle IO -> ReqSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger parserh reqh f = do
  let handle = Handle {
    hLogger = logger,
    hReq = reqh,
    hParser = parserh,

    updateDoc = \doc -> Doc.updateDoc reqh doc
  }
  f handle