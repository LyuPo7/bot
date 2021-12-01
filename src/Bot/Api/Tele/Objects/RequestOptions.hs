module Bot.Api.Tele.Objects.RequestOptions where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B

import qualified Bot.Api.Tele.Objects.CopyMessage as TeleCopyMessage
import qualified Bot.Api.Tele.Objects.GetUpdates as TeleGetUpdates
import qualified Bot.Api.Tele.Objects.KeyboardMessage as TeleKeyboardMessage
import qualified Bot.Api.Tele.Objects.SendMessage as TeleSendMessage
import qualified Bot.Api.Tele.Objects.SetCommands as TeleSetCommands

data RequestOptions
  = GetUpdates TeleGetUpdates.GetUpdates
  | SendMessage TeleSendMessage.SendMessage
  | SendKeyboard TeleKeyboardMessage.KeyboardMessage
  | CopyMessage TeleCopyMessage.CopyMessage
  | SetCommands TeleSetCommands.SetCommands
  deriving (Show, Eq)

encodeRequestOptions :: RequestOptions -> B.ByteString
encodeRequestOptions (GetUpdates option) = encode option
encodeRequestOptions (SendMessage option) = encode option
encodeRequestOptions (SendKeyboard option) = encode option
encodeRequestOptions (CopyMessage option) = encode option
encodeRequestOptions (SetCommands option) = encode option
