{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.Data where

import qualified Web.FormUrlEncoded as Url

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..),
                         genericToJSON, defaultOptions,
                         fieldLabelModifier, genericParseJSON)

import Bot.Vk.Parser.Data

-- | type for RequestOptions requests
newtype VkRequest = VkRequest {
  getRequest :: Text
}

data GetLongPollServer = GetLongPollServer {
    pollServer_groupId :: Integer, -- Id of bot group.
    pollServer_accessToken :: Text, -- Token.
    pollServer_v :: Text -- Version of api.
} deriving (Generic)

instance FromJSON GetLongPollServer where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON GetLongPollServer where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm GetLongPollServer where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm GetLongPollServer where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data LongPollServer = LongPollServer {
    longServer_key :: Text, -- Key for requests.
    longServer_ts :: Integer, -- Server time stamp.
    longServer_act :: Text -- Action.
} deriving (Generic)

instance FromJSON LongPollServer where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON LongPollServer where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm LongPollServer where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm LongPollServer where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data SendMessage = SendMessage {
    sendMessag_accessToken :: Text, -- 
    sendMessag_userId :: Integer, -- Message reciever ID.
    sendMessag_message :: Text, -- Message text.
    sendMessag_v :: Text, -- Api version.
    sendMessag_attachment :: Maybe Text, -- Attachments.
    sendMessag_stickerId :: Maybe Integer, -- Sticker id.
    sendMessag_lat :: Maybe Double, -- Geographical latitude.
    sendMessag_long :: Maybe Double -- 	Geographical longitude.
} deriving (Generic)

instance FromJSON SendMessage where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON SendMessage where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm SendMessage where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm SendMessage where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data GetUploadLink = GetUploadLink {
    getUplLink_type :: Text, -- Type of document: "doc", ... .
    getUplLink_peerId :: Integer, -- Community ID..
    getUplLink_accessToken :: Text, -- Token
    getUplLink_v :: Text -- Api version
} deriving (Generic)

instance FromJSON GetUploadLink where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON GetUploadLink where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm GetUploadLink where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm GetUploadLink where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data SaveDoc = SaveDoc {
    saveDocSer_file :: Text, -- Returned file is uploaded to the server.
    saveDocSer_accessToken :: Text, -- Token.
    saveDocSer_v :: Text -- Api version.
} deriving (Generic)

instance FromJSON SaveDoc where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON SaveDoc where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm SaveDoc where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm SaveDoc where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

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

-- | Default methods for Requests
getPollServer :: GroupID -> Token -> Version -> GetLongPollServer
getPollServer groupId token vkVersion = GetLongPollServer { 
  pollServer_groupId = groupId,
  pollServer_accessToken = token,
  pollServer_v = vkVersion
}

defaultMessage :: UserID -> Token -> Version -> SendMessage
defaultMessage userId token vkVersion = SendMessage { 
  sendMessag_accessToken = token,
  sendMessag_userId = userId,
  sendMessag_message = "",
  sendMessag_v = vkVersion,
  sendMessag_attachment = Nothing,
  sendMessag_stickerId = Nothing,
  sendMessag_lat = Nothing,
  sendMessag_long = Nothing
}

getLink :: PeerId -> FileType -> Version -> Token -> GetUploadLink
getLink peerId fileType vkVersion token = GetUploadLink {
  getUplLink_peerId = peerId,
  getUplLink_type = fileType,
  getUplLink_v = vkVersion,
  getUplLink_accessToken = token
}

saveNewDoc :: FilePathT -> Token -> Version -> SaveDoc
saveNewDoc file token vkVersion = SaveDoc {
  saveDocSer_file = file,
  saveDocSer_accessToken = token,
  saveDocSer_v = vkVersion
}