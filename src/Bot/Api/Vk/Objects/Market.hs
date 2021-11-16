{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Market where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data Market = Market {
  id :: BotSynonyms.MarketId,
  owner_id :: BotSynonyms.UserId
} deriving (Show, Read, Eq, Generic, FromJSON)