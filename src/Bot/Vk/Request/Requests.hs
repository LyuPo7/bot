{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

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
    
    ReqSpec.readFile = \fp -> Prelude.readFile fp,
    makeRequest = \req text -> ReqImpl.makeRequest logger req text,
    getUpdate = \t1 t2 int -> ReqImpl.getUpdate logger t1 t2 int
  }
  f handle