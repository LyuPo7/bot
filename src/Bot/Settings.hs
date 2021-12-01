{-# LANGUAGE DeriveGeneric #-}

module Bot.Settings where

import Data.Aeson.Types (FromJSON)
import qualified Data.Aeson.Types as A
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.Synonyms as BotSynonyms

data Config = Config
  { botApi :: BotApi.Api,
    botToken :: BotSynonyms.Token,
    botInitialReplyNumber :: BotSynonyms.RepNum,
    botQuestion :: Text,
    botDescription :: BotSynonyms.Description,
    botGroupId :: Maybe BotSynonyms.GroupId
  }
  deriving (Show, Generic, Eq)

instance FromJSON Config where
  parseJSON = A.withObject "Config Api" $ \o -> do
    api <- o A..: "bot_api"
    token <- o A..: "bot_token"
    repNum <- o A..: "bot_initial_reply_number"
    question <- o A..: "bot_question"
    description <- o A..: "bot_description"
    groupId <- o A..:? "bot_group_id"
    case (api :: Text) of
      "vk" ->
        return $
          Config
            BotApi.Vk
            token
            repNum
            question
            description
            groupId
      "telegram" ->
        return $
          Config
            BotApi.Tele
            token
            repNum
            question
            description
            groupId
      invalidApi -> A.parserThrowError [] $ show invalidApi

apiTele, apiVk :: BotSynonyms.Host
apiTele = BotSynonyms.Host "https://api.telegram.org/bot"
apiVk = BotSynonyms.Host "https://api.vk.com/method/"

vkVersion :: BotSynonyms.Version
vkVersion = BotSynonyms.Version "5.81"

configFile :: FilePath
configFile = "data/config.json"

timeout :: BotSynonyms.Timeout
timeout = 25
