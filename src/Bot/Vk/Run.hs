{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Run where

import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBQueries as DB
import qualified Bot.Vk.Request.Requests as Req
import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import Bot.Vk.RunSpec (Handle(..))

withHandleIO :: Logger.Handle IO -> Settings.Config -> DBSpec.Handle IO -> ReqSpec.Handle IO -> ParserSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger config dbh reqh parserh f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbh,
    hReq = reqh,
    hParser = parserh,
    
    parseUpdateData = \bstr -> Parser.parseUpdateData parserh bstr,
    parsePollResponse = \bstr -> Parser.parsePollResponse parserh bstr,
    parseUploadUrl = \bstr -> Parser.parseUploadUrl parserh bstr,
    parseUploadFile = \bstr -> Parser.parseUploadFile parserh bstr,
    parseUploadObject = \bstr -> Parser.parseUploadObject parserh bstr,
    
    getLastSucUpdate = DB.getLastSucUpdate dbh,
    putUpdate = \updateId -> DB.putUpdate dbh updateId,
    getRepliesNumber = \userId -> DB.getRepliesNumber dbh userId,
    setRepliesNumber = \userId repNum -> DB.setRepliesNumber dbh userId repNum,
    getMode = \userId -> DB.getMode dbh userId,
    setMode = \userId mode -> DB.setMode dbh userId mode,

    sendNEchoMessage = \userId text mAtts mGeo repNum -> Req.sendNEchoMessage reqh userId text mAtts mGeo repNum,
    sendRepeatMessage = \userId -> Req.sendRepeatMessage reqh userId,
    sendHelpMessage = \userId -> Req.sendHelpMessage reqh userId,
    updateAttachments = \mAtts -> Req.updateAttachments reqh mAtts,
    getUpdate = \text1 text2 int -> Req.getUpdate reqh text1 text2 int,
    getServer = Req.getServer reqh
  }
  f handle