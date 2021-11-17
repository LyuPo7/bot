module Bot.Objects.Method where

import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Vk.Objects.Method as VkMethod

data Method = TeleMethod {
                teleMethod :: TeleMethod.Method
              }
            | VkMethod {
                vkMethod :: VkMethod.Method
              }
            deriving (Show, Eq)