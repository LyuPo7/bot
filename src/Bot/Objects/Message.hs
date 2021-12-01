module Bot.Objects.Message where

import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Vk.Objects.Message as VkMessage

data Message
  = TeleMessage TeleMessage.Message
  | VkMessage VkMessage.Message
  deriving (Show, Eq)
