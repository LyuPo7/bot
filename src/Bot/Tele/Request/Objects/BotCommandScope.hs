{-# LANGUAGE DeriveGeneric #-}

module Bot.Tele.Request.Objects.BotCommandScope where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..), fieldLabelModifier,
                         genericToJSON, defaultOptions)

newtype BotCommandScope = BotCommandScopeDefault {
  scope_type :: Text -- must be default.
} deriving (Show, Generic)

instance ToJSON BotCommandScope where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 6 }

defaultBotCommandScope :: BotCommandScope
defaultBotCommandScope = BotCommandScopeDefault {
  scope_type = "default"
}