module TestBot.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified TestBot.Api.Tele.GenData as TeleGD
import qualified TestBot.Api.Vk.GenData as VkGD

import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.Document as BotDoc

genBotDoc :: Gen BotDoc.Document
genBotDoc = BotDoc.VkDoc
  <$> VkGD.genDoc

genBotTeleUpdate :: Gen BotUpdate.Update
genBotTeleUpdate = BotUpdate.TeleUpdate
  <$> (toInteger <$> TeleGD.genId)

genBotVkUpdate :: Gen BotUpdate.Update
genBotVkUpdate = BotUpdate.VkUpdate
  <$> VkGD.genServer

genBotUpdate :: Gen BotUpdate.Update
genBotUpdate = do
  teleUpdate <- genBotTeleUpdate
  vkUpdate <- genBotVkUpdate
  Gen.element [teleUpdate, vkUpdate]

genBotTeleMessage :: Gen BotMessage.Message
genBotTeleMessage = BotMessage.TeleMessage
  <$> TeleGD.genMessage

genBotTeleNum5Message :: Gen BotMessage.Message
genBotTeleNum5Message = BotMessage.TeleMessage
  <$> TeleGD.genNum5Message

genBotTeleHelpMessage :: Gen BotMessage.Message
genBotTeleHelpMessage = BotMessage.TeleMessage
  <$> TeleGD.genBotHelpMessage

genBotTeleRepeatMessage :: Gen BotMessage.Message
genBotTeleRepeatMessage = BotMessage.TeleMessage
  <$> TeleGD.genBotRepeatMessage

genBotTeleMessageWoBotCom :: Gen BotMessage.Message
genBotTeleMessageWoBotCom = BotMessage.TeleMessage
  <$> TeleGD.genMessageWoBotCom

genBotVkMessageWoBotCom :: Gen BotMessage.Message
genBotVkMessageWoBotCom = BotMessage.VkMessage
  <$> VkGD.genMessageWoBotCom

genBotVkMessage :: Gen BotMessage.Message
genBotVkMessage = BotMessage.VkMessage
  <$> VkGD.genMessage

genBotMessage :: Gen BotMessage.Message
genBotMessage = do
  teleMessage <- genBotTeleMessage
  vkMessage <- genBotVkMessage
  Gen.element [teleMessage, vkMessage]