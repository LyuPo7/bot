{-# LANGUAGE OverloadedStrings #-}

module TestBot.Handlers where

import Control.Monad.Identity

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Request.AttachSpec as AttachSpec
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.DocumentSpec as DocSpec
import Bot.Vk.Parser.Data

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}

reqH :: ReqSpec.Handle Identity
reqH = ReqSpec.Handle {
    ReqSpec.hLogger = logH,
    ReqSpec.configReq = runC,
    ReqSpec.hParser = parserH,

    ReqSpec.readFile = \_ -> Identity "",
    ReqSpec.makeRequest = \_ _ -> Identity "",
    ReqSpec.getUpdate = \_ _ _-> Identity "" 
}

runC :: Settings.Config
runC = Settings.Config {
    Settings.botApi = "telegram",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to recieve?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Nothing
}

attachH1 :: AttachSpec.Handle Identity
attachH1 = AttachSpec.Handle {
    AttachSpec.hLogger = logH,
    AttachSpec.hReq = reqH,
    AttachSpec.hParser = parserH,

    AttachSpec.updateDoc = \doc -> Identity doc
}

attachH2 :: AttachSpec.Handle Identity
attachH2 = AttachSpec.Handle {
    AttachSpec.hLogger = logH,
    AttachSpec.hReq = reqH,
    AttachSpec.hParser = parserH,

    AttachSpec.updateDoc = \doc -> Identity doc {
        document_id = 123,
        document_ownerId = 555,
        document_url = "https://server/link/222"
      }
}

docH1 :: DocSpec.Handle Identity
docH1 = DocSpec.Handle {
    DocSpec.hLogger = logH,
    DocSpec.hReq = reqH,
    DocSpec.hParser = parserH,

    DocSpec.getTemporaryDirectory = Identity "/temp/dir35/",
    DocSpec.saveUploadedDoc = \_ -> Identity "{\"response\":[{\"id\":123,\"owner_id\":12321,\n \"url\":\"https:\\/\\/lp.vk.com\\/link\\/12gh56\"\n}]}",
    DocSpec.downloadFile = \_ _ -> Identity (),
    DocSpec.getUploadedServer = \_ _ -> Identity "{\"response\": {\"upload_url\" : \"https:\\/\\/lp.vk.com\\/link\\/12gh56\"}}",
    DocSpec.uploadFile = \_ _ -> Identity "{\"file\" : \"testFile\"}"
  }