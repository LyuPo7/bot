module TestBot.Api.Tele.Handlers where

import qualified TestBot.Handlers as BotHandlers

import qualified Bot.DB.DB as BotDB
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Request.Request as BotReq
import qualified Bot.Settings as Settings
import qualified Bot.System.System as BotSystem

dbH :: BotDB.Handle Maybe
dbH =
  BotHandlers.dbH
    { BotDB.cDb = runC,
      BotDB.hSystem = sysH
    }

sysH :: BotSystem.Handle Maybe
sysH =
  BotHandlers.sysH
    { BotSystem.cSet = runC
    }

reqH :: BotReq.Handle Maybe
reqH =
  BotHandlers.reqH
    { BotReq.hDb = dbH,
      BotReq.cReq = runC
    }

runC :: Settings.Config
runC =
  Settings.Config
    { Settings.botApi = BotApi.Tele,
      Settings.botToken = BotSynonyms.Token "abcd0dcba",
      Settings.botInitialReplyNumber = 3,
      Settings.botQuestion = "How many replies do you prefer to receive?",
      Settings.botDescription = BotSynonyms.Description "Hi! I'm bot=)",
      Settings.botGroupId = Nothing
    }
