module TestBot.Tele.Handlers where

import Control.Monad.Identity (Identity(..))
import Database.HDBC.Sqlite3 (Connection)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import qualified Bot.Tele.Request.RequestsSpec as ReqSpec
import qualified Bot.Tele.RunSpec as RunSpec
import qualified Bot.Tele.Parser.Objects.UpdateData as UpData

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hConfig = Logger.Config {Logger.cVerbosity = Nothing}
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH,
    ParserSpec.cPar = runC
}

runC :: Settings.Config
runC = Settings.Config {
    Settings.botApi = "telegram",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to receive?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Nothing
}

runCVk :: Settings.Config
runCVk = Settings.Config {
    Settings.botApi = "vk",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to receive?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Just 37891
}

conn :: Connection
conn = undefined

dbH :: DBSpec.Handle Identity
dbH = DBSpec.Handle {
    DBSpec.hLogger = logH,
    DBSpec.hDb = conn,
    DBSpec.configDb = runC
}

reqH :: ReqSpec.Handle Identity
reqH = ReqSpec.Handle {
    ReqSpec.hLogger = logH,
    ReqSpec.hParser = parserH,
    ReqSpec.configReq = runC,

    ReqSpec.makeRequest = \_ _ -> Identity ""
}

runH :: RunSpec.Handle Identity
runH = RunSpec.Handle {
    RunSpec.hLogger = logH,
    RunSpec.cRun = runC,
    RunSpec.hDb = dbH,
    RunSpec.hReq = reqH,
    RunSpec.hParser = parserH,
    
    RunSpec.parseUpdateData = \_ -> return 
      $ UpData.UpdateData {
          UpData.ok = True,
          UpData.result = []
        },
    
    RunSpec.getLastSucUpdate = return (Just 100),
    RunSpec.putUpdate = \_ -> return (),
    RunSpec.getRepliesNumber = return 5,
    RunSpec.setRepliesNumber = \_ _ -> return (),
    RunSpec.getMode = \_ -> return Settings.reply,
    RunSpec.setMode = \_ _ -> return (),

    RunSpec.getUpdate = \_ -> return "{\"ok\":true,\"result\":[]}",
    RunSpec.sendTextMessage = \_ _ -> return (),
    RunSpec.sendEchoMessage = \_ _ -> return (),
    RunSpec.sendNEchoMessage = \_ _ _ -> return (),
    RunSpec.sendQueryNumber = \_ _ -> return "5",
    RunSpec.setCommands = return ()
}