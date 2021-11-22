module TestBot.Api.Tele.Handlers where

import Database.HDBC.Sqlite3 (Connection)

import qualified TestBot.Handlers as BotHandlers

import qualified Bot.Settings as Settings
import qualified Bot.DB.DB as BotDB
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.System.System as BotSystem
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Request.Request as BotReq
import qualified Bot.Mode.Mode as BotMode
import qualified Bot.Objects.Mode as Mode
import qualified Bot.Api.Tele.Request.Requests as TeleReq
import qualified Bot.Api.Tele.Mode.Mode as TeleMode

parserH :: BotParser.Handle Maybe
parserH = BotParser.Handle {
  BotParser.hLogger = BotHandlers.logH,
  BotParser.hSystem = sysH,
  BotParser.cParser = runC
}

conn :: Connection
conn = undefined

dbH :: BotDB.Handle Maybe
dbH = BotDB.Handle {
  BotDB.hLogger = BotHandlers.logH,
  BotDB.conn = conn,
  BotDB.cDb = runC
}

dbqH :: BotDBQ.Handle Maybe
dbqH = BotDBQ.Handle {
  BotDBQ.hLogger = BotHandlers.logH,
  BotDBQ.hDb = dbH,

  BotDBQ.getLastSucUpdate = return (Just 100),
  BotDBQ.putUpdate = \_ -> return (),
  BotDBQ.getRepliesNumber = \_ -> return 5,
  BotDBQ.setRepliesNumber = \_ _ -> return (),
  BotDBQ.getMode = \_ -> return Mode.ReplyMode,
  BotDBQ.setMode = \_ _ -> return ()
}

sysH :: BotSystem.Handle Maybe
sysH = BotSystem.Handle {
  BotSystem.hLogger = BotHandlers.logH,
  BotSystem.cSet = runC,
  
  BotSystem.readFile = \_ -> return "File content",
  BotSystem.getTemporaryDirectory = return "/tempDir",
  BotSystem.downloadFile = \_ _ -> return (),
  BotSystem.uploadFile = \_ _ -> return "{\"file\" : \"testFile\"}"
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

reqH :: BotReq.Handle Maybe
reqH = BotReq.Handle {
  BotReq.hLogger = BotHandlers.logH,
  BotReq.hDb = dbqH,
  BotReq.hParser = parserH,
  BotReq.cReq = runC,

  BotReq.makeRequest = \_ _ -> return "ok",
  BotReq.setUploadedServer = const Nothing,
  BotReq.setUploadedDoc = const Nothing,
  BotReq.setGetServer = Nothing,
  BotReq.setGetUpdate = TeleReq.setGetUpdate parserH,
  BotReq.setEchoMessage = TeleReq.setEchoMessage parserH,
  BotReq.setHelpMessage = TeleReq.setTextMessage parserH,
  BotReq.setStartMessage = TeleReq.setTextMessage parserH,
  BotReq.setKeyboardMessage = TeleReq.setKeyboardMessage parserH,
  BotReq.setCommands = TeleReq.setCommands parserH,

  BotReq.downloadDoc = \_ _ -> Nothing,
  BotReq.extractDoc = const Nothing,
  BotReq.changeMessage = \message _-> return message,
  BotReq.changeDoc = \doc _ -> return doc
}

modeH :: BotMode.Handle Maybe
modeH = BotMode.Handle {
  BotMode.hLogger = BotHandlers.logH,
  BotMode.cRun = runC,
  BotMode.hDb = dbqH,
  BotMode.hReq = reqH,
  BotMode.hParser = parserH,
  
  BotMode.setupBot = TeleMode.setupBot reqH,
  BotMode.getFirstUpdate = TeleMode.getFirstUpdate reqH,
  BotMode.getLastUpdate = TeleMode.getLastUpdate reqH,
  BotMode.getNextUpdate = TeleMode.getNextUpdate reqH,
  BotMode.getPrevUpdate = TeleMode.getPrevUpdate reqH,
  BotMode.getChatId = TeleMode.getChatId reqH,
  BotMode.getMessageText = TeleMode.getMessageText reqH,
  BotMode.getMessageType = TeleMode.getMessageType reqH
}