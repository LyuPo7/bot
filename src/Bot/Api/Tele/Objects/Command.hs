{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.Command where

import Data.Aeson.Types (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Command = Command
  { command :: Text, -- 1-32 characters.
    description :: Text -- 3-256 characters.
  }
  deriving (Show, Eq, Generic, ToJSON)

helpCommand :: Command
helpCommand =
  Command
    { command = "/help",
      description = "Info message"
    }

repCommand :: Command
repCommand =
  Command
    { command = "/repeat",
      description = "Repeat settings"
    }
