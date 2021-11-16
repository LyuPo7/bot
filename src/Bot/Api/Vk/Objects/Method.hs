module Bot.Api.Vk.Objects.Method where

import Data.Text (Text)

import Bot.Settings as Settings

newtype Method = Method {
  getMethod :: Text
}

getLongPollServer :: Method
getLongPollServer = Method $ hostApi <> method
  where hostApi = Settings.getHost Settings.apiVk
        method = "groups.getLongPollServer"

sendMessage :: Method
sendMessage = Method $ hostApi <> method
  where hostApi = Settings.getHost Settings.apiVk
        method = "messages.send"

getMessagesUploadServer :: Method
getMessagesUploadServer = Method $ hostApi <> method
  where hostApi = Settings.getHost Settings.apiVk
        method = "docs.getMessagesUploadServer"

saveDoc :: Method
saveDoc = Method $ hostApi <> method
  where hostApi = Settings.getHost Settings.apiVk
        method = "docs.save"

createPoll :: Method
createPoll = Method $ hostApi <> method 
  where hostApi = Settings.getHost Settings.apiVk
        method = "polls.create"

getUpdate :: Method
getUpdate = Method ""