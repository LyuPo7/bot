{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.GetUpdates where

import Data.Aeson.Types (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Settings as Settings

data GetUpdates = GetUpdates
  { offset :: Maybe BotSynonyms.UpdateId,
    limit :: Maybe BotSynonyms.RecordLimit,
    timeout :: Maybe BotSynonyms.Timeout,
    allowed_updates :: Maybe [Text]
  }
  deriving (Show, Eq, Generic, ToJSON)

createGetUpdates ::
  Maybe BotSynonyms.UpdateId ->
  GetUpdates
createGetUpdates updateIdM =
  GetUpdates
    { offset = updateIdM,
      limit = Nothing,
      timeout = Just Settings.timeout,
      allowed_updates = Just ["message"]
    }
