module Bot.Api.Tele.Objects.Method where

import Data.Text (Text)

newtype Method = Method {
  getMethod :: Text
} deriving (Show, Eq)

getUpdates :: Method
getUpdates = Method "/getUpdates"

sendMessage :: Method
sendMessage = Method "/sendMessage"

copyMessage :: Method
copyMessage = Method "/copyMessage"

getCommands :: Method
getCommands = Method "/getMyCommands"

setCommands :: Method
setCommands = Method "/setMyCommands"