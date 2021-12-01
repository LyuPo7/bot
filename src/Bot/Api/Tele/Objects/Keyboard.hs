{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.Keyboard where

import Data.Aeson.Types (ToJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Api.Tele.Objects.Button as TeleButton

data Keyboard = Keyboard
  { keyboard :: [[TeleButton.Button]],
    resize_keyboard :: Maybe Bool,
    one_time_keyboard :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

createKeyboard ::
  [[TeleButton.Button]] ->
  Keyboard
createKeyboard buttons =
  Keyboard
    { keyboard = buttons,
      one_time_keyboard = Just True,
      resize_keyboard = Just True
    }
