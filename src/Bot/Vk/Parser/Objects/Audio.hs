{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Audio where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (AudioId, UserId)

data Audio = Audio {
  id :: AudioId,
  owner_id :: UserId
} deriving (Show, Read, Eq, Generic, FromJSON)