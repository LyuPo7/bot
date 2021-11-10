{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Geo where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

newtype Geo = Geo {
  coordinates :: Text
} deriving (Show, Read, Eq, Generic, FromJSON)