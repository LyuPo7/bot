{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Market where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Market = Market
  { id :: BotSynonyms.MarketId,
    owner_id :: BotSynonyms.UserId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
