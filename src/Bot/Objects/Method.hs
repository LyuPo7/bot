module Bot.Objects.Method where

import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Vk.Objects.Method as VkMethod

data Method = TeleMethod TeleMethod.Method
            | VkMethod VkMethod.Method
            deriving (Show, Eq)