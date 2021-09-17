{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Run where

import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBQueries as DB
import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.AttachSpec as AttachSpec
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import Bot.Vk.RunSpec (Handle(..))

withHandleIO :: Logger.Handle IO -> Settings.Config -> DBSpec.Handle IO -> ReqSpec.Handle IO -> ParserSpec.Handle IO -> AttachSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger config dbh reqh parserh attachh f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbh,
    hReq = reqh,
    hParser = parserh,
    hAttach = attachh,
    
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

    sendNEchoMessage = \userId text mAtts mGeo repNum -> ReqSpec.sendNEchoMessage reqh userId text mAtts mGeo repNum,
    sendRepeatMessage = \userId -> ReqSpec.sendRepeatMessage reqh userId,
    sendHelpMessage = \userId -> ReqSpec.sendHelpMessage reqh userId,
    getUpdate = \text1 text2 int -> ReqSpec.getUpdate reqh text1 text2 int,
    getServer = ReqSpec.getServer reqh,

    updateAttachments = \mAtts -> AttachSpec.updateAttachments attachh mAtts
  }
  f handle