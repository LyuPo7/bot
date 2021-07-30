{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Bot.Vk.Parser.Data where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

-- | UpdateData
data UpdateData = UpdateData {
  ts :: Text, -- Time stamp.
  updates :: [Update] -- Array of Updates.
} deriving (Show, Generic, FromJSON, ToJSON)

-- | Update
data Update = Update {
  update_type :: Text, -- Update type: "new_message", "reply_message", ... .
  update_object :: Message, -- Message of Update.
  update_groupId :: Integer, -- Identifier of the group of update.
  update_eventId :: Text -- Identifier of the update.
 } deriving (Show,Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Update where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | Users
data Users = Users {
  user_id :: Integer, -- user ID. 
  user_firstName :: Text, -- User's first name.
  user_lastName :: Text, -- User's last name
  user_deactivated :: Text, -- returns if a profile is deleted or blocked. Gets the value deleted or banned. Keep in mind that in this case no additional fields are returned.
  user_isClosed :: Bool, -- true if user profile is closed by his privacy settings.
  user_canAccessClosed :: Bool -- True, if the bot can be invited to groups. Returned only in getMe.
  } deriving (Show,Generic)

instance FromJSON Users where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Users where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Message
data Message = Message {
  message_messageId :: Maybe Integer, -- Message ID. (Not returned for forwarded messages).
  message_date :: Integer, -- date (in Unixtime) when the message was sent.
  message_userId :: Integer, -- Message author ID.
  message_readState :: Maybe Integer, -- Message status (0 — not read, 1 — read). (Not returned for forwarded messages.)
  message_out :: Maybe Integer, -- Message type (0 — received, 1 — sent). (Not returned for forwarded messages.)
  message_title :: Maybe Text, -- Title of message or chat. 
  message_body :: Text, -- message text.
  message_geo :: Maybe Geo, -- Information about location.  
  message_attachments :: Maybe [Attachment], -- Array of media-attachments.
  message_fwdMessages :: Maybe [Message], -- Array of forwarded messages (if any).
  message_emoji :: Maybe Integer, -- Whether the message contains smiles (0 — no, 1 — yes).
  message_important :: Maybe Integer, -- Whether the message is marked as important (0 — no, 1 — yes).
  message_deleted :: Maybe Integer, -- Whether the message is deleted (0 — no, 1 — yes). 
  message_randomId :: Maybe Integer -- Parameter used while sending the message to avoid double sending. 
  } deriving (Show,Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Geo
data Geo = Geo {
  geo_type :: Text, -- location type.
  geo_coordinates :: Text, -- location coordinates.
  geo_place :: Maybe Place -- place description (if added).
  } deriving (Show,Generic)

instance FromJSON Geo where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 4 }

instance ToJSON Geo where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 4 }

-- | Place
data Place = Place {
  place_id :: Maybe Integer, -- place ID (if available).
  place_title :: Maybe Text, -- place title (if available).
  place_latitude :: Maybe Integer, -- geographical latitude.
  place_longitude :: Maybe Integer, -- geographical longitude.
  place_created :: Maybe Integer, -- date when the place has been created (if available).
  place_icon :: Maybe Text, -- URL of icon image.
  place_country :: Maybe Text, -- country name.
  place_cityName :: Maybe Text -- city name.
  } deriving (Show,Generic)

instance FromJSON Place where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Place where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Attachment
data Attachment = Attachment {
    attach_type :: Text, -- type of attachment.
    attach_photo :: Maybe Photo, -- Attached Photo.
    attach_video :: Maybe Video, -- attached Video.
    attach_audio :: Maybe Audio, -- Attached Audio.
    attach_doc :: Maybe Document, -- Attached Document.
    attach_link :: Maybe Link, -- Attached Link.
    attach_marketItem :: Maybe Market,  -- Attached Market.
    attach_marketCollection :: Maybe MarketAlbum, -- Attached MarketAlbum.
    attach_wallPost :: Maybe Wall, -- Attached Wall post.
    attach_wallComment :: Maybe WallReply, -- Attached Wall comment.
    attach_sticker :: Maybe Sticker, -- Attached Sticker.
    attach_gift :: Maybe Gift -- Attached Gift.
} deriving (Show,Generic)

instance FromJSON Attachment where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Attachment where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | Photo
data Photo = Photo {
  photo_id :: Integer, -- Photo ID.
  photo_albumId :: Integer, -- Photo album ID.
  photo_ownerId :: Integer, -- Photo owner ID.
  photo_userId :: Maybe Integer, -- ID of the user who uploaded the photo (if the photo is uploaded in community).
  photo_text :: Text, -- Description text.
  photo_date :: Integer, -- Date when the photo has been added in Unixtime. 
  photo_width :: Integer, -- Width of the original photo in px. 
  photo_height :: Integer, -- Height of the original photo in px.
  photo_accessKey :: Text -- Field is returned with objects which could have specific privacy settings. 
  } deriving (Show,Generic) 

instance FromJSON Photo where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Photo where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Video
data Video = Video {
  video_id :: Integer, -- Video ID.
  video_ownerId :: Integer, -- ID of the user or community that owns the video.
  video_title :: Text, -- Video title.
  video_description :: Text, -- Description of the video. 
  video_duration :: Integer, -- Duration of the video in seconds as defined by sender.
  video_accessKey :: Text -- Field is returned with objects which could have specific privacy settings. 
  } deriving (Show,Generic) 

instance FromJSON Video where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Video where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Audio
data Audio = Audio {
  audio_id :: Integer, -- Audio ID.
  audio_ownerId :: Integer, -- Audio owner ID.
  audio_title :: Text, -- Audio title.
  audio_artist :: Text, -- Artist name.
  audio_duration :: Integer, -- Duration (in seconds).
  audio_url :: Text -- Link to mp3 file. Note that links are bound to an IP address.
  } deriving (Show,Generic)

instance FromJSON Audio where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Audio where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Document
data Document = Document {
  document_id :: Integer, -- Document ID.
  document_ownerId :: Integer, -- Document owner ID.
  document_title :: Text, -- Document title.
  document_size :: Integer, -- Document size.
  document_ext :: Text, -- Document extension.
  document_date :: Integer, -- Date when the document has been added in Unixtime.
  document_duration :: Maybe Integer, -- Duration (in seconds).
  document_url :: Text, -- Link to file. Note that links are bound to an IP address.
  document_accessKey :: Text -- Field is returned with objects which could have specific privacy settings. 
  } deriving (Show,Generic) 

instance FromJSON Document where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON Document where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | Link
data Link = Link {
  link_url :: Integer, -- Link URL.
  link_title :: Text, -- Link title.
  link_description :: Integer -- Link description.
  } deriving (Show,Generic) 

instance FromJSON Link where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5 }

instance ToJSON Link where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 5 }

-- | Market
data Market = Market {
  market_id :: Integer, -- Market ID.
  market_ownerId :: Integer, -- Market owner ID.
  market_title :: Text -- Market title.
  } deriving (Show,Generic) 

instance FromJSON Market where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Market where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | MarketCollection
data MarketAlbum = MarketAlbum {
  marketAl_id :: Integer, -- Market Collection ID.
  marketAl_ownerId :: Integer, -- Market Collection owner ID.
  marketAl_title :: Text -- Market Collection title.
  } deriving (Show,Generic) 

instance FromJSON MarketAlbum where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

instance ToJSON MarketAlbum where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

-- | Wall
data Wall = Wall {
  wall_id :: Integer, -- Wall ID.
  wall_ownerId :: Integer -- Wall owner ID.
  } deriving (Show,Generic) 

instance FromJSON Wall where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Wall where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | WallReply
data WallReply = WallReply {
  wallRep_id :: Integer, -- Comment ID. 
  wallRep_ownerId :: Integer -- Comment author ID. 
  } deriving (Show,Generic) 

instance FromJSON WallReply where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON WallReply where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Sticker
newtype Sticker = Sticker {
  sticker_id :: Integer -- Sticker ID. 
  } deriving (Show,Generic) 

instance FromJSON Sticker where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 8 }

instance ToJSON Sticker where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 8 }

-- | Gift
newtype Gift = Gift {
  gift_id :: Integer -- Gift ID. 
  } deriving (Show,Generic) 

instance FromJSON Gift where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5 }

instance ToJSON Gift where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 5 }

-- | Server
data Server = Server {
  server_key :: Text, -- Key for requests.
  server_server :: Text, -- Requests server.
  server_ts :: Integer  -- Server time stamp.
  } deriving (Show,Generic) 

instance FromJSON Server where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 7 }

instance ToJSON Server where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 7 }

-- | PollResponse
newtype PollResponse = PollResponse {
  pollResponse_response :: Server -- Server.
  } deriving (Show,Generic) 

instance FromJSON PollResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 13 }

instance ToJSON PollResponse where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 13 }

-- | UploadUrlResponse
newtype UploadUrlResponse = UploadUrlResponse {
  upUrlResponse_response :: Maybe UploadUrl -- Url for uploading resource.
  } deriving (Show,Generic) 

instance FromJSON UploadUrlResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 14 }

instance ToJSON UploadUrlResponse where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 14 }

-- | UploadUrl
newtype UploadUrl = UploadUrl {
  upUrl_uploadUrl :: Text -- Url.
  } deriving (Show,Generic) 

instance FromJSON UploadUrl where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON UploadUrl where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }
  
-- | UploadFileResponse
newtype UploadFileResponse = UploadFileResponse {
  upFileResponse_file :: Maybe Text -- Uploade file responce
  } deriving (Show,Generic) 

instance FromJSON UploadFileResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 15 }

instance ToJSON UploadFileResponse where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 15 }

-- | UploadObjectResponse
newtype UploadObjectResponse = UploadObjectResponse {
  upObjResponse_response :: [UploadObject] -- Upload objects.
  } deriving (Show,Generic) 

instance FromJSON UploadObjectResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 14 }

instance ToJSON UploadObjectResponse where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 14 }
  
-- | UploadObject
data UploadObject = UploadObject {
  upObj_id :: Integer, -- Object ID.
  upObj_ownerId :: Integer, -- Object owner ID.
  upObj_title :: Maybe Text, -- Object title.
  upObj_size :: Maybe Integer, -- Object size
  upObj_ext :: Maybe Text, -- Object extension.
  upObj_date :: Integer, -- Date when the Object has been added in Unixtime.
  upObj_type :: Maybe Integer, -- Object type.
  upObj_url :: Text -- Link to Object. Note that links are bound to an IP address.
  } deriving (Show,Generic) 

instance FromJSON UploadObject where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON UploadObject where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Keyboard
data Keyboard = Keyboard {
  keyboard_oneTime :: Maybe Bool, -- Hides the keyboard after the initial use.
  keyboard_buttons :: [[Button]], -- An array of button arrays.
  keyboard_inline :: Maybe Bool -- Shows the keyboard inside the message.(In this case, one_time parameter is not supported)
} deriving (Show,Generic)

instance FromJSON Keyboard where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON Keyboard where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | Button
data Button = Button {
  button_action :: Action, -- An object that describes the type of action and its parameters.
  button_color :: Maybe Text -- Button color. This parameter is used only for buttons with the text type <- [primary,secondary,negative,positive]
} deriving (Show,Generic)

instance FromJSON Button where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Button where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | Action
data Action = Action {
  action_type :: Text, -- "callback", "text", ... .
  action_label :: Text, -- Button text. It is sent by the user to the chat after pressing
  action_payload :: Text -- Additional information. It is returned in the messages_new event inside the payload property.
} deriving (Show,Generic)

instance FromJSON Action where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Action where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | KeyboardData
newtype KeyboardData = KeyboardData {
  keyboard :: Maybe Keyboard -- Keyboard responce.
} deriving (Show, Generic, FromJSON, ToJSON)