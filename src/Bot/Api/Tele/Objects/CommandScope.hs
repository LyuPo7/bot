{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.CommandScope where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), fieldLabelModifier,
                         genericToJSON, defaultOptions)

newtype CommandScope = CommandScope {
  scope_type :: Text -- must be "default".
} deriving (Show, Eq, Generic)

instance ToJSON CommandScope where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 6 }

defaultCommandScope :: CommandScope
defaultCommandScope = CommandScope {
  scope_type = "default"
}