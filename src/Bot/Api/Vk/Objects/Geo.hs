{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Geo where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Geo = Geo
  { coordinates :: Text
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
