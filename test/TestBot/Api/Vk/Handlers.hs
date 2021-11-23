module TestBot.Api.Vk.Handlers where

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
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Api.Vk.Request.Requests as VkReq
import qualified Bot.Api.Vk.Mode.Mode as VkMode

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
    Settings.botApi =  BotApi.Vk,
    Settings.botToken = BotSynonyms.Token "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to receive?",
    Settings.botDescription = BotSynonyms.Description "Hi! I'm bot=)",
    Settings.botGroupId = Just 37891
}

reqH :: BotReq.Handle Maybe
reqH = BotReq.Handle {
  BotReq.hLogger = BotHandlers.logH,
  BotReq.hDb = dbqH,
  BotReq.hParser = parserH,
  BotReq.cReq = runC,

  BotReq.createRequest = \_ _ -> return ("https://api.vk.com", "ok"),
  BotReq.setUploadedServer = VkReq.setUploadedServer parserH,
  BotReq.setUploadedDoc = VkReq.setUploadedDoc parserH,
  BotReq.setGetServer = VkReq.setGetServer parserH,
  BotReq.setGetUpdate = VkReq.setGetUpdate parserH,
  BotReq.setEchoMessage = VkReq.setEchoMessage parserH,
  BotReq.setHelpMessage = VkReq.setHelpMessage parserH,
  BotReq.setStartMessage = \_ _ -> Nothing,
  BotReq.setKeyboardMessage = VkReq.setKeyboardMessage parserH,
  BotReq.setCommands = Nothing,

  BotReq.downloadDoc = VkReq.downloadDoc parserH,
  BotReq.extractDoc = VkReq.extractDoc parserH,
  BotReq.changeMessage = VkReq.changeMessage parserH,
  BotReq.changeDoc = VkReq.changeDoc parserH,

  BotReq.newManager = \_ -> return undefined,
  BotReq.httpLbs = \ _ _ -> return (BotHandlers.resp  "ok")
}

modeH :: BotMode.Handle Maybe
modeH = BotMode.Handle {
  BotMode.hLogger = BotHandlers.logH,
  BotMode.cRun = runC,
  BotMode.hDb = dbqH,
  BotMode.hReq = reqH,
  BotMode.hParser = parserH,
  
  BotMode.setupBot = return (),
  BotMode.getFirstUpdate = VkMode.getFirstUpdate reqH,
  BotMode.getLastUpdate = VkMode.getLastUpdate reqH,
  BotMode.getNextUpdate = VkMode.getNextUpdate reqH,
  BotMode.getPrevUpdate = VkMode.getPrevUpdate reqH,
  BotMode.getChatId = VkMode.getChatId reqH,
  BotMode.getMessageText = VkMode.getMessageText reqH,
  BotMode.getMessageType = VkMode.getMessageType reqH
}