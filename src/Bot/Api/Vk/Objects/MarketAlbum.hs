{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.MarketAlbum where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data MarketAlbum = MarketAlbum
  { id :: BotSynonyms.MarketAlbumId,
    owner_id :: BotSynonyms.UserId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
