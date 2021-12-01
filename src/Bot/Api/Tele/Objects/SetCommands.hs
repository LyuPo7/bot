{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.SetCommands where

import Data.Aeson.Types (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Api.Tele.Objects.Command as TeleCommand
import qualified Bot.Api.Tele.Objects.CommandScope as TeleCommandScope

data SetCommands = SetCommands
  { commands :: [TeleCommand.Command],
    scope :: Maybe TeleCommandScope.CommandScope,
    language_code :: Maybe Text -- A two-letter ISO 639-1 language code.
  }
  deriving (Show, Eq, Generic, ToJSON)

createCommands :: SetCommands
createCommands =
  SetCommands
    { commands = [TeleCommand.helpCommand, TeleCommand.repCommand],
      scope = Just TeleCommandScope.defaultCommandScope,
      language_code = Just ""
    }
