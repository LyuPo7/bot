module TestBot.Api.Vk.Handlers where

import Database.HDBC.Sqlite3 (Connection)

import qualified TestBot.Handlers as BotHandlers

import qualified Bot.Settings as Settings
import qualified Bot.DB.DB as BotDB
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.System.System as BotSystem
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Mode as Mode
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi

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
  BotDBQ.hSystem = sysH,
  BotDBQ.cDBQ = runC,

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
  BotReq.cReq = runC,

  BotReq.newManager = \_ -> return undefined,
  BotReq.httpLbs = \ _ _ -> return (BotHandlers.resp "ok")
}