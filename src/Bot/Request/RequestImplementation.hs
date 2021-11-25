module Bot.Request.RequestImplementation where

import  qualified Network.HTTP.Client as HTTPClient

import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Request.Request as BotReq
import qualified Bot.Settings as Settings

withHandleIO :: Logger.Handle IO ->
                BotDBQ.Handle IO ->
                Settings.Config ->
               (BotReq.Handle IO -> IO a) ->
                IO a
withHandleIO logger dbH config f = do
  let handle = BotReq.Handle {
    BotReq.hLogger = logger,
    BotReq.hDb = dbH,
    BotReq.cReq = config,

    BotReq.newManager = HTTPClient.newManager,
    BotReq.httpLbs = HTTPClient.httpLbs
  }
  f handle