module Bot.Objects.Document where

import qualified Bot.Api.Vk.Objects.Document as VkDoc

newtype Document = VkDoc {
  vkDoc :: VkDoc.Document
}