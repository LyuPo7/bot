{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.UpdateData where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Api.Tele.Objects.Update as TeleUpdate
import qualified Bot.Objects.Synonyms as BotSynonyms

data UpdateData = UpdateData
  { ok :: BotSynonyms.Status,
    result :: [TeleUpdate.Update]
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
