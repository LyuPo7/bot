module Bot.Objects.Button where

import Data.Text (Text)

import qualified Bot.Api.Tele.Objects.Button as TeleButton

data Button = Button {
  text :: Text,
  description :: Text
}

createTeleButton :: Button -> TeleButton.Button
createTeleButton botButton = TeleButton.createButton
  (text botButton) (description botButton)