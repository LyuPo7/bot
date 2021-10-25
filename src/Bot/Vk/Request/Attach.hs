{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Request.Attach where

import Bot.Vk.Request.AttachSpec (Handle(..))
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.DocumentSpec as DocSpec
import qualified Bot.Logger as Logger

withHandleIO :: Logger.Handle IO -> ParserSpec.Handle IO -> ReqSpec.Handle IO ->
                DocSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger parserh reqh doch f = do
  let handle = Handle {
    hLogger = logger,
    hReq = reqh,
    hParser = parserh,

    updateDoc = DocSpec.updateDoc doch
  }
  f handle