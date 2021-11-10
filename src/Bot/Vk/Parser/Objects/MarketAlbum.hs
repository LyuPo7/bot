{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.MarketAlbum where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (MarketAlbumId, UserId)

data MarketAlbum = MarketAlbum {
  id :: MarketAlbumId,
  owner_id :: UserId
} deriving (Show, Read, Eq, Generic, FromJSON)