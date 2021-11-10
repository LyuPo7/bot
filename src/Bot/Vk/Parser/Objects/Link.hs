{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Link where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (Url)

newtype Link = Link {
  url :: Url
} deriving (Show, Read, Eq, Generic, FromJSON)