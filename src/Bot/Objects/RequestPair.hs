module Bot.Objects.RequestPair where

import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Tele.Objects.RequestOptions as TeleReqOptions
import qualified Bot.Api.Vk.Objects.Method as VkMethod
import qualified Bot.Api.Vk.Objects.RequestOptions as VkReqOptions

data ReqPair
  = TeleReqPair (TeleMethod.Method, TeleReqOptions.RequestOptions)
  | VkReqPair (VkMethod.Method, VkReqOptions.RequestOptions)
  deriving (Show, Eq)
