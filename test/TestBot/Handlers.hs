module TestBot.Handlers where

import Database.HDBC.Sqlite3 (Connection)

import qualified Data.ByteString.Lazy as B
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Types as HTTPTypes

import qualified Bot.DB.DB as BotDB
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.Mode as Mode
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Request.Request as BotReq
import qualified Bot.Settings as Settings
import qualified Bot.System.System as BotSystem

logH :: Logger.Handle Maybe
logH =
  Logger.Handle
    { Logger.log = \_ _ -> return (),
      Logger.hConfig = Logger.Config {Logger.cVerbosity = Nothing}
    }

resp :: B.ByteString -> HTTPInternal.Response B.ByteString
resp text =
  HTTPInternal.Response
    { HTTPInternal.responseStatus = status,
      HTTPInternal.responseBody = text,
      HTTPInternal.responseVersion = undefined,
      HTTPInternal.responseHeaders = undefined,
      HTTPInternal.responseCookieJar = undefined,
      HTTPInternal.responseClose' = undefined
    }

status :: HTTPTypes.Status
status =
  HTTPTypes.Status
    { HTTPTypes.statusCode = 200,
      HTTPTypes.statusMessage = ""
    }

conn :: Connection
conn = undefined

dbH :: BotDB.Handle Maybe
dbH =
  BotDB.Handle
    { BotDB.hLogger = logH,
      BotDB.conn = conn,
      BotDB.hSystem = sysH,
      BotDB.cDb = runDefault
    }

sysH :: BotSystem.Handle Maybe
sysH =
  BotSystem.Handle
    { BotSystem.hLogger = logH,
      BotSystem.cSet = runDefault,
      BotSystem.readFile = \_ -> return "File content",
      BotSystem.getTemporaryDirectory = return "/tempDir",
      BotSystem.downloadFile = \_ _ -> return (),
      BotSystem.uploadFile = \_ _ -> return "{\"file\" : \"testFile\"}"
    }

reqH :: BotReq.Handle Maybe
reqH =
  BotReq.Handle
    { BotReq.hLogger = logH,
      BotReq.hDb = dbH,
      BotReq.cReq = runDefault,
      BotReq.newManager = \_ -> return undefined,
      BotReq.httpLbs = \_ _ -> return (resp "ok"),
      BotReq.getLastSucUpdate = return (Just 100),
      BotReq.putUpdate = \_ -> return (),
      BotReq.getRepliesNumber = \_ -> return 5,
      BotReq.setRepliesNumber = \_ _ -> return (),
      BotReq.getMode = \_ -> return Mode.ReplyMode,
      BotReq.setMode = \_ _ -> return ()
    }

runDefault :: Settings.Config
runDefault =
  Settings.Config
    { Settings.botApi = BotApi.Tele,
      Settings.botToken = BotSynonyms.Token "abcd0dcba",
      Settings.botInitialReplyNumber = 3,
      Settings.botQuestion = "How many replies do you prefer to receive?",
      Settings.botDescription = BotSynonyms.Description "Hi! I'm bot=)",
      Settings.botGroupId = Nothing
    }
