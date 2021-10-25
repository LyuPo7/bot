{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Run where

import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBQueries as DB
import qualified Bot.Tele.Parser.Parser as Parser
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Tele.Request.RequestsSpec as ReqSpec
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.RunSpec (Handle(..))

withHandleIO :: Logger.Handle IO -> Settings.Config -> DBSpec.Handle IO ->
                ReqSpec.Handle IO -> ParserSpec.Handle IO ->
                (Handle IO -> IO a) -> IO a
withHandleIO logger config dbh reqh parserh f = do
  let handle = Handle {
    hLogger = logger,
    cRun = config,
    hDb = dbh,
    hReq = reqh,
    hParser = parserh,
    
    parseUpdateData = Parser.parseUpdateData parserh,
    
    getLastSucUpdate = DB.getLastSucUpdate dbh,
    putUpdate = DB.putUpdate dbh,
    getRepliesNumber = DB.getRepliesNumber dbh,
    setRepliesNumber = DB.setRepliesNumber dbh,
    getMode = DB.getMode dbh,
    setMode = DB.setMode dbh,

    getUpdate = ReqSpec.getUpdate reqh,
    sendTextMessage = ReqSpec.sendTextMessage reqh,
    sendEchoMessage = ReqSpec.sendEchoMessage reqh,
    sendNEchoMessage = ReqSpec.sendNEchoMessage reqh,
    sendQueryNumber = ReqSpec.sendQueryNumber reqh,
    setCommands = ReqSpec.setCommands reqh
  }
  f handle