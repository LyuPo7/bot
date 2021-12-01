{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.Button where

import Data.Aeson.Types (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Button = Button
  { text :: Text,
    callback_data :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

createButton ::
  Text ->
  Text ->
  Button
createButton buttonText callback =
  Button
    { text = buttonText,
      callback_data = Just callback
    }
