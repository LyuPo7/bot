module Bot.Objects.RequestOptions where

import qualified Bot.Api.Tele.Objects.RequestOptions as TeleReqOptions
import qualified Bot.Api.Vk.Objects.RequestOptions as VkReqOptions

data RequestOptions = TeleReqOptions {
                        teleReqOptions :: TeleReqOptions.RequestOptions
                      }
                    | VkReqOptions {
                        vkReqOptions :: VkReqOptions.RequestOptions
                      }