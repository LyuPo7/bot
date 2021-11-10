module TestBot.Tele.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Tele.Parser.Objects.UpdateData as UpData
import qualified Bot.Tele.Parser.Objects.Update as Update
import qualified Bot.Tele.Parser.Objects.Message as Message
import qualified Bot.Tele.Parser.Objects.MessageEntity as MessageEntity
import qualified Bot.Tele.Parser.Objects.Chat as Chat

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genChat :: Gen Chat.Chat
genChat = Chat.Chat
  <$> (toInteger <$> genId)

genMessageEntity :: Gen MessageEntity.MessageEntity
genMessageEntity = MessageEntity.MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "bot_command", "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessageEntityWoBotCom :: Gen MessageEntity.MessageEntity
genMessageEntityWoBotCom = MessageEntity.MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessage :: Gen Message.Message
genMessage = Message.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genNum5Message :: Gen Message.Message
genNum5Message = Message.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Just "5"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genBotHelpMessage :: Gen Message.Message
genBotHelpMessage = Message.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Just "/start", Just "/help"]
  <*> Gen.constant (Just [MessageEntity.MessageEntity "bot_command"])

genBotRepeatMessage :: Gen Message.Message
genBotRepeatMessage = Message.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.constant (Just "/repeat")
  <*> Gen.constant (Just [MessageEntity.MessageEntity "bot_command"])

genMessageWoBotCom :: Gen Message.Message
genMessageWoBotCom = Message.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntityWoBotCom)

genUpdate :: Gen Update.Update
genUpdate = Update.Update
  <$> (toInteger <$> genId)
  <*> Gen.maybe genMessage

genUpdateData :: Gen UpData.UpdateData
genUpdateData = UpData.UpdateData
  <$> Gen.bool
  <*> Gen.list (Range.constant 0 10) genUpdate