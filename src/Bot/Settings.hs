{-# LANGUAGE DeriveGeneric #-}

module Bot.Settings where

import qualified Data.Aeson.Types as A
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Aeson (camelTo2, parseJSON, toJSON)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Config = Config {
  botApi :: BotSynonyms.Api,
  botToken :: BotSynonyms.Token,
  botInitialReplyNumber :: BotSynonyms.RepNum,
  botQuestion :: Text,
  botDescription :: BotSynonyms.Description,
  botGroupId :: Maybe BotSynonyms.GroupId
} deriving (Show, Generic, Eq)

instance FromJSON Config where
  parseJSON = A.genericParseJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' }

instance ToJSON Config where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' }

newtype Host = Host { getHost :: Text }

-- | Bot commands
startMessage, helpMessage, repeatMessage :: Text
startMessage = "/start" -- initialize chat with new User;
helpMessage = "/help"   -- request bot description;
repeatMessage = "/repeat" -- request for change reply number;

-- | Bot modes
reply, answer :: BotSynonyms.Mode
reply = "reply" -- in this mode: Bot replies for every User's message;
answer = "answer" -- in this mode: Bot tries to receive new reply number;

-- | Api host
apiTele, apiVk :: Host
apiTele = Host "https://api.telegram.org/bot"
apiVk = Host "https://api.vk.com/method/"

-- | Api version
vkVersion :: BotSynonyms.Version
vkVersion = "5.81"

-- | Config file
configFile :: FilePath
configFile = "data/config.json"

-- | Timeout
timeout :: BotSynonyms.Timeout
timeout = 25