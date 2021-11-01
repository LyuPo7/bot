module TestBot.Tele.Run where

import Control.Monad.Identity (Identity(..))

import Hedgehog (Property, property, (===), forAll)

import TestBot.Tele.GenData as GD
import TestBot.Tele.Handlers as H

import qualified Bot.Settings as Settings
import qualified Bot.Tele.RunSpec as RunSpec

prop_answerModeFail :: Property
prop_answerModeFail = property $ do
  message <- forAll GD.genMessage
  RunSpec.answerMode H.runH message === Identity Nothing

prop_answerModeSuc :: Property
prop_answerModeSuc = property $ do
  message <- forAll GD.genNum5Message
  RunSpec.answerMode H.runH message === Identity (Just 5)

prop_replyModeReplySuc :: Property
prop_replyModeReplySuc = property $ do
  message <- forAll GD.genBotHelpMessage
  RunSpec.replyMode H.runH message === Identity Settings.reply

prop_replyModeAnswerSuc :: Property
prop_replyModeAnswerSuc = property $ do
  message <- forAll GD.genBotRepeatMessage
  RunSpec.replyMode H.runH message === Identity Settings.answer

prop_replyModeOrdinarySuc :: Property
prop_replyModeOrdinarySuc = property $ do
  message <- forAll GD.genMessageWoBotCom
  RunSpec.replyMode H.runH message === Identity Settings.reply