{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Document where

import Control.Monad.Identity

import qualified Bot.Settings as Settings
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Request.DocumentSpec as DocSpec
import qualified Bot.Logger as Logger
import qualified Bot.Vk.Parser.Data as DParser

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec ()
import Test.Hspec

runC :: Settings.Config
runC = Settings.Config {
    Settings.botApi = "telegram",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to recieve?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Nothing
}

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
}

reqH :: ReqSpec.Handle Identity
reqH = ReqSpec.Handle {
    ReqSpec.hLogger = logH,
    ReqSpec.configReq = runC,
    ReqSpec.hParser = parserH
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}

handle1 :: DocSpec.Handle Identity
handle1 = DocSpec.Handle {
    DocSpec.hLogger = logH,
    DocSpec.hReq = reqH,
    DocSpec.hParser = parserH,

    DocSpec.getTemporaryDirectory = Identity "/temp/dir35/",
    DocSpec.saveUploadedDoc = \_ -> Identity "{\"response\":[{\"id\":123,\"owner_id\":12321,\n \"url\":\"https:\\/\\/lp.vk.com\\/link\\/12gh56\"\n}]}",
    DocSpec.downloadFile = \_ _ -> Identity (),
    DocSpec.getUploadedServer = \_ _ -> Identity "{\"response\": {\"upload_url\" : \"https:\\/\\/lp.vk.com\\/link\\/12gh56\"}}",
    DocSpec.uploadFile = \_ _ -> Identity "{\"file\" : \"testFile\"}"
  }

genDoc :: Gen DParser.Document
genDoc = DParser.Document
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["book.pdf", "log.txt", "prog.py"]
  <*> Gen.element ["https://server/link/123", "https://server/link/222"]
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

spec_updateDoc :: Spec
spec_updateDoc= describe "Testing updateDoc" $ do
    it "Should successfully change Document if saveUploadedDoc return 1 correct Object" $ do
      doc <- Gen.sample genDoc
      let result = DocSpec.updateDoc handle1 doc
      result `shouldBe` (Identity doc {DParser.document_url = "https://lp.vk.com/link/12gh56", DParser.document_id = 123, DParser.document_ownerId = 12321})
    it "Should successfully return Document without changes if saveUploadedDoc return more than 1 correct Object" $ do
      doc <- Gen.sample genDoc
      let handle2 = handle1 {DocSpec.saveUploadedDoc = \_ -> Identity "{\"response\" : [\n{\n\"id\":123,\n\"owner_id\":12321,\n\"url\":\"https:\\/\\/lp.vk.com\\/link\\/12gh56\"\n},\n{\n\"id\":555,\n\"owner_id\":909089,\n\"url\":\"https:\\/\\/lp.vk.com\\/link\\/ad1f2ghjk\"\n}\n]}"}
          result = DocSpec.updateDoc handle2 doc
      result `shouldBe` (Identity doc)
    it "Should successfully return Document without changes if saveUploadedDoc return uncorrect Object" $ do
      doc <- Gen.sample genDoc
      let handle2 = handle1 {DocSpec.saveUploadedDoc = \_ -> Identity "{\"response\" : []}"}
          result = DocSpec.updateDoc handle2 doc
      result `shouldBe` (Identity doc)
    