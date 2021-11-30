module Bot.Request.RequestImplementation where

import  qualified Network.HTTP.Client as HTTPClient

import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DB as BotDB
import qualified Bot.DB.DBImplementation as BotDBImpl
import qualified Bot.Request.Request as BotReq
import qualified Bot.Settings as Settings

withHandleIO :: Logger.Handle IO ->
                BotDB.Handle IO ->
                Settings.Config ->
               (BotReq.Handle IO -> IO a) ->
                IO a
withHandleIO logger dbH config f = do
  let handle = BotReq.Handle {
    BotReq.hLogger = logger,
    BotReq.hDb = dbH,
    BotReq.cReq = config,

    BotReq.newManager = HTTPClient.newManager,
    BotReq.httpLbs = HTTPClient.httpLbs,

    BotReq.getLastSucUpdate = BotDBImpl.getLastSucUpdate dbH,
    BotReq.putUpdate = BotDBImpl.putUpdate dbH,
    BotReq.getRepliesNumber = BotDBImpl.getRepliesNumber dbH,
    BotReq.setRepliesNumber = BotDBImpl.setRepliesNumber dbH,
    BotReq.getMode = BotDBImpl.getMode dbH,
    BotReq.setMode = BotDBImpl.setMode dbH
  }
  f handle