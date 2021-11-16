{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Tele.Objects.UpdateData where

import Data.Aeson.Types (FromJSON(..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate

data UpdateData = UpdateData {
  ok :: BotSynonyms.Status,
  result :: [TeleUpdate.Update]
} deriving (Show, Read, Eq, Generic, FromJSON)