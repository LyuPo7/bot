{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Tele.Objects.GetUpdates where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

import qualified Bot.Settings as Settings
import qualified Bot.Objects.Synonyms as BotSynonyms

data GetUpdates = GetUpdates {
  offset :: Maybe BotSynonyms.UpdateId,
  limit :: Maybe BotSynonyms.RecordLimit,
  timeout :: Maybe BotSynonyms.Timeout,
  allowed_updates :: Maybe [Text]
} deriving (Show, Eq, Generic, ToJSON)

createGetUpdates :: BotSynonyms.UpdateId ->
                    GetUpdates
createGetUpdates updateId = GetUpdates {
  offset = Just updateId,
  limit = Nothing,
  timeout = Just Settings.timeout,
  allowed_updates = Just ["message"]
}