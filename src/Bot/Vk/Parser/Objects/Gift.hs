{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Gift where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (GiftId)

newtype Gift = Gift {
  id :: GiftId
} deriving (Show, Read, Eq, Generic, FromJSON)