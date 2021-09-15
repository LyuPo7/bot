{-# LANGUAGE OverloadedStrings #-}

module TestBot.Tele.Run where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy.Char8 as L8

import Database.HDBC.Sqlite3 (Connection)
import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Tele.Request.RequestsSpec as ReqSpec
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.RunSpec()
import qualified Bot.Tele.Parser.Data as DParser

import Hedgehog (Gen, Property, property, (===), forAll)
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import Control.Monad.Identity()

import Bot.Tele.Parser.Data
import Bot.Tele.RunSpec

prop_answerModeFail :: Property
prop_answerModeFail = property $ do
  message <- forAll genMessage
  answerMode handle message === Identity Nothing

prop_answerModeSuc :: Property
prop_answerModeSuc = property $ do
  message <- forAll genNum5Message
  answerMode handle message === (Identity $ Just 5)

prop_replyModeReplySuc :: Property
prop_replyModeReplySuc = property $ do
  message <- forAll genBotHelpMessage
  replyMode handle message === (Identity Settings.reply)

prop_replyModeAnswerSuc :: Property
prop_replyModeAnswerSuc = property $ do
  message <- forAll genBotReplyMessage
  replyMode handle message === (Identity Settings.answer)

prop_replyModeOrdinarySuc :: Property
prop_replyModeOrdinarySuc = property $ do
  message <- forAll genMessageWoBotCom
  replyMode handle message === (Identity Settings.reply)

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genChat :: Gen Chat
genChat = Chat
  <$> (toInteger <$> genId)

genMessageEntity :: Gen MessageEntity
genMessageEntity = MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "bot_command", "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessageEntityWoBotCom :: Gen MessageEntity
genMessageEntityWoBotCom = MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessage :: Gen Message
genMessage = Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genNum5Message :: Gen Message
genNum5Message = Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Just "5"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genBotHelpMessage :: Gen Message
genBotHelpMessage = Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Just "/start", Just "/help"]
  <*> Gen.constant (Just [MessageEntity "bot_command"])

genBotReplyMessage :: Gen Message
genBotReplyMessage = Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.constant (Just "/repeat")
  <*> Gen.constant (Just [MessageEntity "bot_command"])

genMessageWoBotCom :: Gen Message
genMessageWoBotCom = Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntityWoBotCom)

genUpdate :: Gen Update
genUpdate = Update
  <$> (toInteger <$> genId)
  <*> Gen.maybe genMessage

genUpdateData :: Gen UpdateData
genUpdateData = UpdateData
  <$> Gen.bool
  <*> Gen.list (Range.constant 0 10) genUpdate

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
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

conn :: Connection
conn = undefined

dbH :: DBSpec.Handle Identity
dbH = DBSpec.Handle {
    DBSpec.hLogger = logH,
    DBSpec.hDb = conn,
    DBSpec.configDb = runC
}

reqH :: ReqSpec.Handle Identity
reqH = ReqSpec.Handle {
    ReqSpec.hLogger = logH,
    ReqSpec.configReq = runC
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}

handle :: Handle Identity
handle = Handle {
    hLogger = logH,
    cRun = runC,
    hDb = dbH,
    hReq = reqH,
    hParser = parserH,
    
    parseUpdateData = \_ -> return $ DParser.UpdateData {DParser.ok = True, DParser.result = []},
    
    getLastSucUpdate = return (Just 100),
    putUpdate = \_ -> return (),
    getRepliesNumber = return 5,
    setRepliesNumber = \_ _ -> return (),
    getMode = \_ -> return (Settings.reply),
    setMode = \_ _ -> return (),

    getUpdate = \_ -> return $ L8.pack "{\"ok\":true,\"result\":[]}",
    sendTextMessage = \_ _ -> return (),
    sendEchoMessage = \_ _ -> return (),
    sendNEchoMessage = \_ _ _ -> return (),
    sendQueryNumber = \_ _ -> return (L8.pack "5"),
    setCommands = return ()
}
  

