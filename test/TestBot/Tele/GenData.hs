module TestBot.Tele.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bot.Tele.Parser.Data

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

genBotRepeatMessage :: Gen Message
genBotRepeatMessage = Message
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