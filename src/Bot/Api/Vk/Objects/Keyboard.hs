{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Keyboard where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Api.Vk.Objects.Button as VkButton

data Keyboard = Keyboard
  { one_ime :: Maybe Bool, -- Hides the keyboard after the initial use.
    buttons :: [[VkButton.Button]],
    inline :: Maybe Bool -- Shows the keyboard inside the message.
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
