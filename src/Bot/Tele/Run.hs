{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Run where

import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBQueries as DB
import qualified Bot.Tele.Request.Requests as Req
import qualified Bot.Tele.Parser.Parser as Parser
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Tele.Request.RequestsSpec as ReqSpec
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.RunSpec (Handle(..))

withHandleIO :: Logger.Handle IO -> Settings.Config -> DBSpec.Handle IO -> ReqSpec.Handle IO -> ParserSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger config dbh reqh parserh f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbh,
    hReq = reqh,
    hParser = parserh,
    
    parseUpdateData = \bstr -> Parser.parseUpdateData parserh bstr,
    
    getLastSucUpdate = DB.getLastSucUpdate dbh,
    putUpdate = \updateId -> DB.putUpdate dbh updateId,
    getRepliesNumber = \chatId -> DB.getRepliesNumber dbh chatId,
    setRepliesNumber = \chatId repNum -> DB.setRepliesNumber dbh chatId repNum,
    getMode = \chatId -> DB.getMode dbh chatId,
    setMode = \chatId mode -> DB.setMode dbh chatId mode,

    getUpdate = \updateId -> Req.getUpdate reqh updateId,
    sendTextMessage = \chatId text -> Req.sendTextMessage reqh chatId text,
    sendEchoMessage = \chatId messageId -> Req.sendEchoMessage reqh chatId messageId,
    sendNEchoMessage = \chatId messageId repNum -> Req.sendNEchoMessage reqh chatId messageId repNum,
    sendQueryNumber = \chatId text -> Req.sendQueryNumber reqh chatId text
  }
  f handle