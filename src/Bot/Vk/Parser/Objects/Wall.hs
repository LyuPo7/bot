{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Wall where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (WallId, UserId)

data Wall = Wall {
  id :: WallId,
  owner_id :: UserId
} deriving (Show, Read, Eq, Generic, FromJSON)