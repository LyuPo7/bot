{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Sticker where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype Sticker = Sticker
  { id :: BotSynonyms.StickerId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
