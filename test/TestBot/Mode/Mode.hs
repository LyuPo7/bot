module TestBot.Mode.Mode where

import Hedgehog (Property, property, (===), forAll)

import qualified TestBot.GenData as BotGD
import qualified TestBot.Api.Vk.Handlers as VkHandlers
import qualified TestBot.Api.Tele.Handlers as TeleHandlers

import qualified Bot.Settings as Settings
import qualified Bot.Mode.Mode as BotMode
import qualified Bot.Objects.Mode as Mode

prop_answerModeFail :: Property
prop_answerModeFail = property $ do
  message <- forAll BotGD.genBotVkMessage
  BotMode.answerMode VkHandlers.modeH message === Just Nothing

prop_answerModeSuc :: Property
prop_answerModeSuc = property $ do
  message <- forAll BotGD.genBotTeleNum5Message
  BotMode.answerMode TeleHandlers.modeH message === Just (Just 5)

prop_replyModeReplySuc :: Property
prop_replyModeReplySuc = property $ do
  message <- forAll BotGD.genBotTeleHelpMessage
  BotMode.replyMode TeleHandlers.modeH message === Just Mode.ReplyMode

prop_replyModeAnswerSuc :: Property
prop_replyModeAnswerSuc = property $ do
  message <- forAll BotGD.genBotTeleRepeatMessage
  BotMode.replyMode TeleHandlers.modeH message === Just Mode.AnswerMode

prop_replyModeOrdinarySuc :: Property
prop_replyModeOrdinarySuc = property $ do
  message <- forAll BotGD.genBotVkMessageWoBotCom
  BotMode.replyMode VkHandlers.modeH message === Just Mode.ReplyMode