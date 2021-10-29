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
withHandleIO logger config dbh reqh parserh attachh f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbh,
    hReq = reqh,
    hParser = parserh,
    hAttach = attachh,
    
    parseUpdateData = Parser.parseUpdateData parserh,
    parsePollResponse = Parser.parsePollResponse parserh,
    parseUploadUrl = Parser.parseUploadUrl parserh,
    parseUploadFile = Parser.parseUploadFile parserh,
    parseUploadObject = Parser.parseUploadObject parserh,
    
    getLastSucUpdate = DB.getLastSucUpdate dbh,
    putUpdate = DB.putUpdate dbh,
    getRepliesNumber = DB.getRepliesNumber dbh,
    setRepliesNumber = DB.setRepliesNumber dbh,
    getMode = DB.getMode dbh,
    setMode = DB.setMode dbh,

    sendNEchoMessage = ReqSpec.sendNEchoMessage reqh,
    sendRepeatMessage = ReqSpec.sendRepeatMessage reqh,
    sendHelpMessage =ReqSpec.sendHelpMessage reqh,
    getUpdate = ReqSpec.getUpdate reqh,
    getServer = ReqSpec.getServer reqh,

    updateAttachments = AttachSpec.updateAttachments attachh
  }
  f handle