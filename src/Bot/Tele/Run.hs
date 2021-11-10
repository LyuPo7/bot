module Bot.Tele.Run where

import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBQueries as DB
import qualified Bot.Tele.Parser.Parser as Parser
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Tele.Request.RequestsSpec as ReqSpec
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.RunSpec (Handle(..))

withHandleIO :: Logger.Handle IO -> Settings.Config -> DBSpec.Handle IO ->
                ReqSpec.Handle IO -> ParserSpec.Handle IO ->
                (Handle IO -> IO a) -> IO a
withHandleIO logger config dbH reqH parserH f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbH,
    hReq = reqH,
    hParser = parserH,
    
    parseUpdateData = Parser.parseUpdateData parserH,
    
    getLastSucUpdate = DB.getLastSucUpdate dbH,
    putUpdate = DB.putUpdate dbH,
    getRepliesNumber = DB.getRepliesNumber dbH,
    setRepliesNumber = DB.setRepliesNumber dbH,
    getMode = DB.getMode dbH,
    setMode = DB.setMode dbH,

    getUpdate = ReqSpec.getUpdate reqH,
    sendTextMessage = ReqSpec.sendTextMessage reqH,
    sendEchoMessage = ReqSpec.sendEchoMessage reqH,
    sendNEchoMessage = ReqSpec.sendNEchoMessage reqH,
    sendQueryNumber = ReqSpec.sendQueryNumber reqH,
    setCommands = ReqSpec.setCommands reqH
  }
  f handle