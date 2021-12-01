{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Gift where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype Gift = Gift
  { id :: BotSynonyms.GiftId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
