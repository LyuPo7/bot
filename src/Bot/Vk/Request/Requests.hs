{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Request.Requests where

import Bot.Vk.Request.RequestsSpec (Handle(..))
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.RequestsImpl as ReqImpl
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings

withHandleIO :: Logger.Handle IO -> Settings.Config -> ParserSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger config parserh f = do
  let handle = Handle {
    hLogger = logger,
    configReq = config,
    hParser = parserh,
    
    ReqSpec.readFile = Prelude.readFile,
    makeRequest = ReqImpl.makeRequest logger,
    getUpdate = ReqImpl.getUpdate logger
  }
  f handle