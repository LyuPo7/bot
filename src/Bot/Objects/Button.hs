module Bot.Objects.Button where

import Data.Text (Text)

import qualified Bot.Api.Tele.Objects.Button as TeleButton
import qualified Bot.Util as BotUtil

data Button = Button {
  text :: Text,
  description :: Text
}

createTeleButton :: Button -> TeleButton.Button
createTeleButton botButton = TeleButton.createButton
  (text botButton) (description botButton)

defaultButton :: Int -> Button
defaultButton num = Button numText $ "Pressed " <> numText
  where numText = BotUtil.convertValue num