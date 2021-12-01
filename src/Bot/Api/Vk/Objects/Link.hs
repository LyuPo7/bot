{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Link where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype Link = Link
  { url :: BotSynonyms.Url
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
