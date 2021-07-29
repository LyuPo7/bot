{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

-- BotConfig
data BotConfig = BotConfig {
    botApi :: Text,
    botToken :: Text,
    botInitialReplyNumber :: Integer,
    botQuestion :: Text,
    botDescription :: Text,
    botGroupId :: Maybe Integer
} deriving (Show,Generic)

instance FromJSON BotConfig where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' }

instance ToJSON BotConfig where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' }