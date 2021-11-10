{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Request.Objects.InlineKeyboardButton where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

data InlineKeyboardButton = InlineKeyboardButton {
  text :: Text,
  callback_data :: Maybe Text
} deriving (Show, Generic, ToJSON)

createButton :: Text -> Text -> InlineKeyboardButton
createButton buttonText callback = InlineKeyboardButton {
  text = buttonText,
  callback_data = Just callback
}