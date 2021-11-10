{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Sticker where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (StickerId)

newtype Sticker = Sticker {
  id :: StickerId
} deriving (Show, Read, Eq, Generic, FromJSON)