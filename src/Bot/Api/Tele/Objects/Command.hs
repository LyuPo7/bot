{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Tele.Objects.Command where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

data Command = Command {
  command :: Text, -- 1-32 characters.
  description :: Text -- 3-256 characters.
} deriving (Show, Generic, ToJSON)

helpCommand :: Command
helpCommand = Command {
  command = "/help",
  description = "Info message"
}

repCommand :: Command
repCommand = Command {
  command = "/repeat",
  description = "Repeat settings"
}