module TestBot.Api.Tele.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.MessageEntity as TeleMessageEntity
import qualified Bot.Api.Tele.Objects.Chat as TeleChat

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genChat :: Gen TeleChat.Chat
genChat = TeleChat.Chat . BotSynonyms.ChatId <$> (toInteger <$> genId)

genMessageEntity :: Gen TeleMessageEntity.MessageEntity
genMessageEntity = TeleMessageEntity.MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "bot_command", "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessageEntityWoBotCom :: Gen TeleMessageEntity.MessageEntity
genMessageEntityWoBotCom = TeleMessageEntity.MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessage :: Gen TeleMessage.Message
genMessage = TeleMessage.Message
  <$> (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genNum5Message :: Gen TeleMessage.Message
genNum5Message = TeleMessage.Message
  <$> (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> genChat
  <*> Gen.element [Just "5"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genBotHelpMessage :: Gen TeleMessage.Message
genBotHelpMessage = TeleMessage.Message
  <$> (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> genChat
  <*> Gen.element [Just "/start", Just "/help"]
  <*> Gen.constant (Just [TeleMessageEntity.MessageEntity "bot_command"])

genBotRepeatMessage :: Gen TeleMessage.Message
genBotRepeatMessage = TeleMessage.Message
  <$> (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> genChat
  <*> Gen.constant (Just "/repeat")
  <*> Gen.constant (Just [TeleMessageEntity.MessageEntity "bot_command"])

genMessageWoBotCom :: Gen TeleMessage.Message
genMessageWoBotCom = TeleMessage.Message
  <$> (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntityWoBotCom)

genUpdate :: Gen TeleUpdate.Update
genUpdate = TeleUpdate.Update
  <$> (BotSynonyms.UpdateId <$> (toInteger <$> genId))
  <*> Gen.maybe genMessage

genUpdateData :: Gen TeleUpData.UpdateData
genUpdateData = TeleUpData.UpdateData
  <$> (BotSynonyms.Status <$> Gen.bool)
  <*> Gen.list (Range.constant 0 10) genUpdate