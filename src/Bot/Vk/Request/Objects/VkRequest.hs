module Bot.Vk.Request.Objects.VkRequest where

import Data.Text (Text)

-- | type for RequestOptions requests
newtype VkRequest = VkRequest {
  getRequest :: Text
}

-- | Vk Requests
getLongPollServer :: VkRequest
getLongPollServer = VkRequest "groups.getLongPollServer"

sendMessage :: VkRequest
sendMessage = VkRequest "messages.send"

getMessagesUploadServer :: VkRequest
getMessagesUploadServer = VkRequest "docs.getMessagesUploadServer"

saveDoc :: VkRequest
saveDoc = VkRequest "docs.save"

createPoll :: VkRequest
createPoll = VkRequest "polls.create"