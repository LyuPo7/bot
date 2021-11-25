{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Tele.Objects.Button where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

data Button = Button {
  text :: Text,
  callback_data :: Maybe Text
} deriving (Show, Eq, Generic, ToJSON)

createButton :: Text ->
                Text ->
                Button
createButton buttonText callback = Button {
  text = buttonText,
  callback_data = Just callback
}