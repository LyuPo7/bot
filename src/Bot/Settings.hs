{-# LANGUAGE DeriveGeneric #-}

module Bot.Settings where

import qualified Data.Aeson.Types as A
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON)
import Data.Aeson (parseJSON)

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi

data Config = Config {
  botApi :: BotApi.Api,
  botToken :: BotSynonyms.Token,
  botInitialReplyNumber :: BotSynonyms.RepNum,
  botQuestion :: Text,
  botDescription :: BotSynonyms.Description,
  botGroupId :: Maybe BotSynonyms.GroupId
} deriving (Show, Generic, Eq)

instance FromJSON Config where
  parseJSON = A.withObject "Config Api"$ \o -> do
    api <- o A..: "bot_api"
    token <- o A..: "bot_token"
    repNum  <- o A..: "bot_initial_reply_number"
    question <- o A..: "bot_question"
    description <- o A..: "bot_description"
    groupId <- o A..:? "bot_group_id"
    case (api :: Text) of
      "vk" -> return $ Config BotApi.Vk
        token repNum question description groupId
      "telegram" -> return $ Config BotApi.Tele
        token repNum question description groupId
      _ -> return $ Config BotApi.InvalidApi
        token repNum question description groupId

newtype Host = Host { getHost :: Text }

-- | Bot commands
startMessage, helpMessage, repeatMessage :: Text
startMessage = "/start" -- initialize chat with new User;
helpMessage = "/help"   -- request bot description;
repeatMessage = "/repeat" -- request for change reply number;

-- | Api host
apiTele, apiVk :: Host
apiTele = Host "https://api.telegram.org/bot"
apiVk = Host "https://api.vk.com/method/"

-- | Api version
vkVersion :: BotSynonyms.Version
vkVersion = BotSynonyms.Version "5.81"

-- | Config file
configFile :: FilePath
configFile = "data/config.json"

-- | Timeout
timeout :: BotSynonyms.Timeout
timeout = 25