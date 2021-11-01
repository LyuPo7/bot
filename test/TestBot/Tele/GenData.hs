module TestBot.Tele.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Tele.Parser.Data as PD

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genChat :: Gen PD.Chat
genChat = PD.Chat
  <$> (toInteger <$> genId)

genMessageEntity :: Gen PD.MessageEntity
genMessageEntity = PD.MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "bot_command", "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessageEntityWoBotCom :: Gen PD.MessageEntity
genMessageEntityWoBotCom = PD.MessageEntity 
  <$> Gen.element ["mention", "hashtag", "cashtag", 
                   "url", "email", 
                   "phone_number", "bold", "italic", 
                   "underline", "strikethrough", "code", "pre",
                   "text_link", "text_mention"]

genMessage :: Gen PD.Message
genMessage = PD.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genNum5Message :: Gen PD.Message
genNum5Message = PD.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Just "5"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntity)

genBotHelpMessage :: Gen PD.Message
genBotHelpMessage = PD.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Just "/start", Just "/help"]
  <*> Gen.constant (Just [PD.MessageEntity "bot_command"])

genBotRepeatMessage :: Gen PD.Message
genBotRepeatMessage = PD.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.constant (Just "/repeat")
  <*> Gen.constant (Just [PD.MessageEntity "bot_command"])

genMessageWoBotCom :: Gen PD.Message
genMessageWoBotCom = PD.Message
  <$> (toInteger <$> genId)
  <*> genChat
  <*> Gen.element [Nothing, Just "Hi!", Just "Let's talk!"]
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genMessageEntityWoBotCom)

genUpdate :: Gen PD.Update
genUpdate = PD.Update
  <$> (toInteger <$> genId)
  <*> Gen.maybe genMessage

genUpdateData :: Gen PD.UpdateData
genUpdateData = PD.UpdateData
  <$> Gen.bool
  <*> Gen.list (Range.constant 0 10) genUpdate