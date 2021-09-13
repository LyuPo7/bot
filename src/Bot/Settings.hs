{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Bot.Settings where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2, parseJSON, toJSON)
import Data.Aeson.Types (ToJSON, FromJSON, genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

import Bot.Tele.Request.Data ()

-- | Bot Config Settings
data Config = Config {
    botApi :: Text,
    botToken :: Text,
    botInitialReplyNumber :: Integer,
    botQuestion :: Text,
    botDescription :: Text,
    botGroupId :: Maybe Integer
} deriving (Show, Generic, Eq)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' }

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' }

-- | Types for Settings
-- | Host
newtype Host = Host { getHost :: Text }

-- | Bot commands
startMessage, helpMessage, repeatMessage :: Text
startMessage = pack "/start" -- initialize chat with new User;
helpMessage = pack "/help"   -- request bot description;
repeatMessage = pack "/repeat" -- request for change reply number;

-- | Bot modes
reply, answer :: Text
reply = pack "reply" -- in this mode: Bot replies for every User's message;
answer = pack "answer" -- in this mode: Bot tries to recieve new reply number from User;

-- | Api host
apiTele, apiVk :: Host
apiTele = Host "https://api.telegram.org/bot"
apiVk = Host "https://api.vk.com/method/"

-- | Api version
vkVersion :: Text
vkVersion = pack "5.80"

-- | Config file
configFile :: FilePath
configFile = "src/Bot/files/confighier_tele.json"
--configFile = "src/Bot/files/confighier_vvk.json"
-- configFile = "src/Bot/files/confighier_vk.json"