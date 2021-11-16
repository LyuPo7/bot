{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Keyboard where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))
  
import qualified Bot.Api.Vk.Objects.Button as VkButton

data Keyboard = Keyboard {
  one_ime :: Maybe Bool, -- Hides the keyboard after the initial use.
  buttons :: [[VkButton.Button]],
  inline :: Maybe Bool -- Shows the keyboard inside the message.
} deriving (Show, Read, Eq, Generic, FromJSON)