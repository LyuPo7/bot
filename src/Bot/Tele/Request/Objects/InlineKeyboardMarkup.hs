{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Request.Objects.InlineKeyboardMarkup where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

import Bot.Tele.Request.Objects.InlineKeyboardButton (InlineKeyboardButton(..))

data InlineKeyboardMarkup = InlineKeyboardMarkup {
  keyboard :: [[InlineKeyboardButton]],
  resize_keyboard :: Maybe Bool,
  one_time_keyboard :: Maybe Bool
} deriving (Show, Generic, ToJSON)

createKeyboard :: [[InlineKeyboardButton]] -> InlineKeyboardMarkup
createKeyboard buttons = InlineKeyboardMarkup {
  keyboard = buttons,
  one_time_keyboard = Just True,
  resize_keyboard = Just True
}