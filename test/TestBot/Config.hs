{-# LANGUAGE OverloadedStrings #-}

module TestBot.Config where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, Property, property, (===), forAll)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bot.Config
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings

prop_checkConfig :: Property
prop_checkConfig = property $ do
      config <- forAll genValidConfig
      checkConfig config === Right config
      
genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genValidLogConfig :: Gen Logger.Config
genValidLogConfig = Logger.Config
  <$> Gen.element [Just Logger.Debug, Just Logger.Info, Just Logger.Warning, Just Logger.Error, Nothing]

genValidSetConfig :: Gen Settings.Config
genValidSetConfig = Settings.Config
  <$> Gen.element ["vk", "telegram"]
  <*> (convert <$> (Gen.string (Range.constant 10 15) Gen.ascii))
  <*> (toInteger <$> (Gen.int (Range.constant 1 5)))
  <*> Gen.element ["Reps?"]
  <*> Gen.element ["Bot!"]
  <*> (Gen.maybe (toInteger <$> (Gen.int (Range.constant 0 1000))))

genValidConfig :: Gen Config
genValidConfig = Config
  <$> genValidSetConfig
  <*> genValidLogConfig

convert :: Show a => a -> Text
convert = T.pack . show