{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Request.Objects.BotCommand where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

data BotCommand = BotCommand {
  command :: Text, -- 1-32 characters.
  description :: Text -- 3-256 characters.
} deriving (Show, Generic, ToJSON)

helpBotCommand :: BotCommand
helpBotCommand = BotCommand {
  command = "/help",
  description = "Info message"
}

repBotCommand :: BotCommand
repBotCommand = BotCommand {
  command = "/repeat",
  description = "Repeat settings"
}