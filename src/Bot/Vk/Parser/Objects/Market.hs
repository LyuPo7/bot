{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Market where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (MarketId, UserId)

data Market = Market {
  id :: MarketId,
  owner_id :: UserId
} deriving (Show, Read, Eq, Generic, FromJSON)