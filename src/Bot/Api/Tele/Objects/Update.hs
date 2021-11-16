{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.Update where

import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON(..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Tele.Objects.Message as TeleMessage

data Update = Update {
  id :: BotSynonyms.UpdateId,
  message :: Maybe TeleMessage.Message
} deriving (Show, Read, Eq, Generic)

instance FromJSON Update where
  parseJSON = A.withObject "Action" $ \o ->
    Update
      <$> o A..: "update_id"
      <*> o A..: "message"