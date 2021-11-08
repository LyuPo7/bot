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

withHandleIO :: Logger.Handle IO -> Settings.Config -> DBSpec.Handle IO ->
                ReqSpec.Handle IO -> ParserSpec.Handle IO ->
                AttachSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger config dbh reqH parserH attachH f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbh,
    hReq = reqH,
    hParser = parserH,
    hAttach = attachH,
    
    parseUpdateData = Parser.parseUpdateData parserH,
    parsePollResponse = Parser.parsePollResponse parserH,
    parseUploadUrl = Parser.parseUploadUrl parserH,
    parseUploadFile = Parser.parseUploadFile parserH,
    parseUploadObject = Parser.parseUploadObject parserH,
    
    getLastSucUpdate = DB.getLastSucUpdate dbh,
    putUpdate = DB.putUpdate dbh,
    getRepliesNumber = DB.getRepliesNumber dbh,
    setRepliesNumber = DB.setRepliesNumber dbh,
    getMode = DB.getMode dbh,
    setMode = DB.setMode dbh,

    sendNEchoMessage = ReqSpec.sendNEchoMessage reqH,
    sendRepeatMessage = ReqSpec.sendRepeatMessage reqH,
    sendHelpMessage =ReqSpec.sendHelpMessage reqH,
    getUpdate = ReqSpec.getUpdate reqH,
    getServer = ReqSpec.getServer reqH,

    updateAttachments = AttachSpec.updateAttachments attachH
  }
  f handle