module Bot.Tele.Request.Objects.TeleRequest where

import Data.Text (Text)

newtype TeleRequest = TeleRequest { getRequest :: Text }

getUpdates :: TeleRequest
getUpdates = TeleRequest "/getUpdates"

sendMessage :: TeleRequest
sendMessage = TeleRequest "/sendMessage"

copyMessage :: TeleRequest
copyMessage = TeleRequest "/copyMessage"

getBotCommands :: TeleRequest
getBotCommands = TeleRequest "/getMyCommands"

setBotCommands :: TeleRequest
setBotCommands = TeleRequest "/setMyCommands"