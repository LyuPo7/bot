{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Handlers where

import Control.Monad.Identity
import Database.HDBC.Sqlite3 (Connection)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Vk.Request.AttachSpec as AttachSpec
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.DocumentSpec as DocSpec
import qualified Bot.Vk.RunSpec as RunSpec
import qualified Bot.Vk.Parser.Data as DParser

conn :: Connection
conn = undefined

dbH :: DBSpec.Handle Identity
dbH = DBSpec.Handle {
    DBSpec.hLogger = logH,
    DBSpec.hDb = conn,
    DBSpec.configDb = runC
}

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}

reqH :: ReqSpec.Handle Identity
reqH = ReqSpec.Handle {
    ReqSpec.hLogger = logH,
    ReqSpec.configReq = runC,
    ReqSpec.hParser = parserH,

    ReqSpec.readFile = \_ -> Identity "",
    ReqSpec.makeRequest = \_ _ -> Identity "",
    ReqSpec.getUpdate = \_ _ _-> Identity "" 
}

runC :: Settings.Config
runC = Settings.Config {
    Settings.botApi = "telegram",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to recieve?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Nothing
}

runCVk :: Settings.Config
runCVk = Settings.Config {
    Settings.botApi = "vk",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to recieve?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Just 37891
}

attachH1 :: AttachSpec.Handle Identity
attachH1 = AttachSpec.Handle {
    AttachSpec.hLogger = logH,
    AttachSpec.hReq = reqH,
    AttachSpec.hParser = parserH,

    AttachSpec.updateDoc = \doc -> Identity doc
}

attachH2 :: AttachSpec.Handle Identity
attachH2 = AttachSpec.Handle {
    AttachSpec.hLogger = logH,
    AttachSpec.hReq = reqH,
    AttachSpec.hParser = parserH,

    AttachSpec.updateDoc = \doc -> Identity doc {
        DParser.document_id = 123,
        DParser.document_ownerId = 555,
        DParser.document_url = "https://server/link/222"
      }
}

docH1 :: DocSpec.Handle Identity
docH1 = DocSpec.Handle {
    DocSpec.hLogger = logH,
    DocSpec.hReq = reqH,
    DocSpec.hParser = parserH,

    DocSpec.getTemporaryDirectory = Identity "/temp/dir35/",
    DocSpec.saveUploadedDoc = \_ -> Identity "{\"response\":[{\"id\":123,\"owner_id\":12321,\n \"url\":\"https:\\/\\/lp.vk.com\\/link\\/12gh56\"\n}]}",
    DocSpec.downloadFile = \_ _ -> Identity (),
    DocSpec.getUploadedServer = \_ _ -> Identity "{\"response\": {\"upload_url\" : \"https:\\/\\/lp.vk.com\\/link\\/12gh56\"}}",
    DocSpec.uploadFile = \_ _ -> Identity "{\"file\" : \"testFile\"}"
  }

runH :: RunSpec.Handle Identity
runH = RunSpec.Handle {
    RunSpec.hLogger = logH,
    RunSpec.cRun = runC,
    RunSpec.hDb = dbH,
    RunSpec.hReq = reqH,
    RunSpec.hParser = parserH,
    RunSpec.hAttach = attachH1,
    
    RunSpec.parsePollResponse = \_ -> return $ Right $ DParser.PollResponse {DParser.pollResponse_response = DParser.ServerText {DParser.serverText_key = "347e47284fc18830341f78af8a14b434b0cf359e", DParser.serverText_server = "https://lp.vk.com/wh205828081", DParser.serverText_ts = "543"}},
    RunSpec.parseUpdateData = \_ -> return $ DParser.UpdateData {DParser.ts = "10", DParser.updates = []},
    RunSpec.parseUploadUrl = \_ -> return $ DParser.UploadUrlResponse {DParser.upUrlResponse_response = Nothing},
    RunSpec.parseUploadFile = \_ -> return $ DParser.UploadFileResponse {DParser.upFileResponse_file = Nothing},
    RunSpec.parseUploadObject = \_ -> return $ DParser.UploadObjectResponse {DParser.upObjResponse_response = []},
    
    RunSpec.getLastSucUpdate = return (Just 100),
    RunSpec.putUpdate = \_ -> return (),
    RunSpec.getRepliesNumber = return 5,
    RunSpec.setRepliesNumber = \_ _ -> return (),
    RunSpec.getMode = \_ -> return (Settings.reply),
    RunSpec.setMode = \_ _ -> return (),

    RunSpec.getUpdate = \_ _ _ -> return "{\"ts\":\"0\",\"updates\":[]}",
    RunSpec.getServer = return "{\"response\":{\"key\":\"347e47284fc18830341f78af8a14b434b0cf359e\",\"server\":\"https://lp.vk.com/wh205828081\",\"ts\":543}}",
    RunSpec.sendHelpMessage = \_ -> return (),
    RunSpec.sendRepeatMessage = \_ -> return (),
    RunSpec.sendNEchoMessage = \_ _ _ _ _ -> return (),
    RunSpec.updateAttachments = \mAtts -> AttachSpec.updateAttachments attachH1 mAtts
}