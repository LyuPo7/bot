module Bot.Objects.Button where

import Data.Text (Text)

data Button = Button {
  text :: Text,
  description :: Text
}