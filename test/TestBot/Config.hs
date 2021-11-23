module TestBot.Config where

import Hedgehog (Gen, Property, property, (===), forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Config as Config
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi

prop_checkConfig :: Property
prop_checkConfig = property $ do
      config <- forAll genValidConfig
      Config.checkConfig config === Right config
      
genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genValidLogConfig :: Gen Logger.Config
genValidLogConfig = Logger.Config
  <$> Gen.element [
        Just Logger.Debug,
        Just Logger.Info,
        Just Logger.Warning,
        Just Logger.Error,
        Nothing]

genValidSetConfig :: Gen Settings.Config
genValidSetConfig = Settings.Config
  <$> Gen.element [BotApi.Vk, BotApi.Tele]
  <*> (BotSynonyms.Token <$> Gen.text (Range.constant 10 15) Gen.ascii)
  <*> (BotSynonyms.RepNum <$> (toInteger <$> Gen.int (Range.constant 1 5)))
  <*> Gen.element ["Reps?"]
  <*> (BotSynonyms.Description <$> Gen.element ["Bot!"])
  <*> Gen.maybe (BotSynonyms.GroupId <$> (toInteger <$> Gen.int (Range.constant 0 1000)))

genValidConfig :: Gen Config.Config
genValidConfig = Config.Config
  <$> genValidSetConfig
  <*> genValidLogConfig