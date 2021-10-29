module Bot.Tele.Request.Requests where

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.Request.RequestsSpec (Handle(..))
import qualified Bot.Tele.Request.RequestsImpl as ReqImpl

withHandleIO :: Logger.Handle IO -> ParserSpec.Handle IO ->
                Settings.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger parserh config f = do
  let handle = Handle {
    hLogger = logger,
    hParser = parserh,
    configReq = config,

    makeRequest = ReqImpl.makeRequest parserh
  }
  f handle