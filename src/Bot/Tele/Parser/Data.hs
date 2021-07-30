{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Bot.Tele.Parser.Data where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

-- | User
data User = User {
  user_id :: Integer, -- Unique identifier for this user or bot. 
  user_isBot :: Bool, -- True, if this user is a bot
  user_firstName :: Text, -- User's or bot's first name
  user_lastName :: Maybe Text, -- User's or bot's last name
  user_username :: Maybe Text, -- User's or bot's username
  user_languageCode :: Maybe Text, -- IETF language tag of the user's language
  user_canJoinGroups :: Maybe Bool, -- True, if the bot can be invited to groups. Returned only in getMe.
  user_canReadAllGroupMessages :: Maybe Bool, -- True, if privacy mode is disabled for the bot. Returned only in getMe.
  user_SupportsInlineQueries :: Maybe Bool --True, if the bot supports inline queries. Returned only in getMe.
  } deriving (Show,Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Chat (only private chats)
data Chat = Chat {
  chat_id :: Integer, -- Unique identifier for this chat.
  chat_type :: Text, -- Type of chat, can be either “private”, “group”, “supergroup” or “channel”.
  chat_title :: Maybe Text, -- Title, for supergroups, channels and group chats.
  chat_username :: Maybe Text, -- Username, for private chats, supergroups and channels if available.
  chat_firstName :: Maybe Text, -- First name of the other party in a private chat.
  chat_lastName :: Maybe Text, -- Last name of the other party in a private chat
  chat_photo :: Maybe ChatPhoto, -- Chat photo. Returned only in getChat.
  chat_bio :: Maybe Text, -- Bio of the other party in a private chat. Returned only in getChat.
  chat_description :: Maybe Text, -- Description, for groups, supergroups and channel chats. Returned only in getChat.
  chat_InviteLink :: Maybe Text, -- Primary invite link, for groups, supergroups and channel chats. Returned only in getChat.
  chat_PinnedMessage :: Maybe Message, -- The most recent pinned message (by sending date). Returned only in getChat.
  chat_permissions :: Maybe ChatPermissions, -- Default chat member permissions, for groups and supergroups. Returned only in getChat.
  chat_slowModeDelay :: Maybe Integer, -- For supergroups, the minimum allowed delay between consecutive messages sent by each unpriviledged user. Returned only in getChat.
  chat_messageAutoDeleteTime :: Maybe Integer, -- The time after which all messages sent to the chat will be automatically deleted; in seconds. Returned only in getChat.
  chat_stickerSetName :: Maybe Text, -- For supergroups, name of group sticker set. Returned only in getChat.
  chat_canSetStickerSet :: Maybe Bool, -- True, if the bot can change the group sticker set. Returned only in getChat.
  chat_linkedChatId :: Maybe ChatLocation -- For supergroups, the location to which the supergroup is connected. Returned only in getChat.
  } deriving (Show,Generic) 

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | ChatPermissions
data ChatPermissions = ChatPermissions {
  chatperm_canSendMessages :: Maybe Bool, -- True, if the user is allowed to send text messages, contacts, locations and venues.
  chatperm_canSendMediaMessages :: Maybe Bool, -- True, if the user is allowed to send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages.
  chatperm_canSendPolls :: Maybe Bool, -- True, if the user is allowed to send polls, implies can_send_messages.
  chatperm_canSendOtherMessages :: Maybe Bool, -- True, if the user is allowed to send animations, games, stickers and use inline bots, implies can_send_media_messages.
  chatperm_canAddWebPagePreviews :: Maybe Bool, -- True, if the user is allowed to add web page previews to their messages, implies can_send_media_messages.
  chatperm_canChangeInfo :: Maybe Bool, -- True, if the user is allowed to change the chat title, photo and other settings. Ignored in public supergroups.
  chatperm_canInviteUsers :: Maybe Bool, -- True, if the user is allowed to invite new users to the chat.
  chatperm_canPinMessages :: Maybe Bool -- True, if the user is allowed to pin messages. Ignored in public supergroups.
  } deriving (Show,Generic) 

instance FromJSON ChatPermissions where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON ChatPermissions where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | ChatLocation
data ChatLocation = ChatLocation {
  chatloc_location :: Location, -- The location to which the supergroup is connected. Can't be a live location.
  chatloc_address :: Text -- Location address.
  } deriving (Show,Generic) 

instance FromJSON ChatLocation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 8 }

instance ToJSON ChatLocation where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 8 }

-- | ChatPhoto
data ChatPhoto = ChatPhoto {
  chatphoto_smallFileId :: Text, -- File identifier of small (160x160) chat photo.
  chatphoto_smallFileUniqueId :: Text, -- Unique file identifier of small (160x160) chat photo.
  chatphoto_bigFileId :: Text, -- File identifier of big (640x640) chat photo.
  chatphoto_bigFileUniqueId :: Text -- Unique file identifier of big (640x640) chat photo.
  } deriving (Show,Generic) 

instance FromJSON ChatPhoto where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

instance ToJSON ChatPhoto where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

-- | PhotoSize
data PhotoSize = PhotoSize {
  photo_fileId :: Text, -- Identifier for this file.
  photo_fileUniqueId :: Text, -- Unique identifier for this file.
  photo_fileSize :: Integer, -- File size.
  photo_width :: Integer, -- Photo width.
  photo_height :: Integer -- Photo height.
  } deriving (Show,Generic) 

instance FromJSON PhotoSize where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON PhotoSize where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Video
data Video = Video {
  video_fileId :: Text, -- Identifier for this file.
  video_fileUniqueId :: Text, -- Unique identifier for this file.
  video_width :: Integer, -- Video width.
  video_height :: Integer, -- Video height.
  video_duration :: Integer, -- Duration of the video in seconds as defined by sender.
  video_thumb :: Maybe PhotoSize, -- Video thumbnail.
  video_fileName :: Maybe Text, -- Original filename as defined by sender,
  video_mimeType :: Maybe Text, -- Mime type of a file as defined by sender.
  video_fileSize :: Maybe Integer -- File size.
  } deriving (Show,Generic) 

instance FromJSON Video where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Video where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | VideoNote
data VideoNote = VideoNote {
  videonote_fileId :: Text, -- Identifier for this file.
  videonote_fileUniqueId :: Text, -- Unique identifier for this file.
  videonote_length :: Integer, -- Video width and height (diameter of the video message) as defined by sender.
  videonote_duration :: Integer, -- Duration of the video in seconds as defined by sender.
  videonote_thumb :: Maybe PhotoSize, -- Video thumbnail.
  videonote_fileSize :: Maybe Integer -- File size.
  } deriving (Show,Generic) 

instance FromJSON VideoNote where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

instance ToJSON VideoNote where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

-- | Sticker
data Sticker = Sticker {
  sticker_fileId :: Text, -- Identifier for this file.
  sticker_fileUniqueId :: Text, -- Unique identifier for this file.
  sticker_width :: Integer, -- Sticker width.
  sticker_height :: Integer, -- Sticker height.
  sticker_isAnimated :: Bool, -- True, if the sticker is animated.
  sticker_thumb :: Maybe PhotoSize, -- Sticker thumbnail in the .WEBP or .JPG format.
  sticker_emoji :: Maybe Text, -- Emoji associated with the sticker.
  sticker_setName :: Maybe Text, -- Name of the sticker set to which the sticker belongs.
  sticker_maskPosition :: Maybe MaskPosition, -- For mask stickers, the position where the mask should be placed.
  sticker_fileSize :: Maybe Integer -- File size.
  } deriving (Show,Generic) 

instance FromJSON Sticker where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Sticker where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | MaskPosition
data MaskPosition = MaskPosition {
  mask_point :: Text, -- The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
  mask_Xshift :: Float, -- Shift by X-axis measured in widths of the mask scaled to the face size, from left to right.
  mask_Yshift :: Float, -- Shift by X-axis measured in widths of the mask scaled to the face size, from top to bottom.
  mask_scale :: Float -- Mask scaling coefficient.
  } deriving (Show,Generic) 

instance FromJSON MaskPosition where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON MaskPosition where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Animation
data Animation = Animation {
  animation_fileId :: Text, -- Identifier for this file.
  animation_fileUniqueId :: Text, -- Unique identifier for this file.
  animation_width :: Integer, -- Video width.
  animation_height :: Integer, -- Video height.
  animation_duration :: Integer, -- Duration of the video in seconds as defined by sender.
  animation_thumb :: Maybe PhotoSize, -- Video thumbnail.
  animation_fileName :: Maybe Text, -- Original filename as defined by sender,
  animation_mimeType :: Maybe Text, -- Mime type of a file as defined by sender.
  animation_fileSize :: Maybe Integer -- File size.
  } deriving (Show,Generic) 

instance FromJSON Animation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

instance ToJSON Animation where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10 }

-- | Venue
data Venue = Venue {
  venue_location :: Location, -- Venue location.
  venue_title :: Text, -- Name of the venue.
  venue_address :: Text, -- Address of the venue.
  venue_foursquareId :: Maybe Text, -- Foursquare identifier of the venue.
  venue_foursquaretype :: Maybe Text, --  Foursquare type of the venue.
  venue_googlePlaceId :: Maybe Text, -- Google Places identifier of the venue.
  venue_google_place_type :: Maybe Text --  Google Places type of the venue.
  } deriving (Show,Generic) 

instance FromJSON Venue where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Venue where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Poll
data Poll = Poll {
  poll_id :: Text, -- Unique poll identifier.
  poll_question :: Text, -- Poll question, 1-300 characters.
  poll_options :: [PollOption], -- List of poll options.
  poll_totalVoterCount :: Integer, -- Total number of users that voted in the poll.
  poll_isClosed :: Bool, -- True, if the poll is closed.
  poll_isAnonymous :: Bool, -- True, if the poll is anonymous.
  poll_type :: Text, -- Poll type, currently can be “regular” or “quiz”.
  poll_AllowsMultipleAnswers :: Bool, -- True, if the poll allows multiple answers.
  poll_correctOptionId :: Maybe Integer, -- 0-based identifier of the correct answer option.
  poll_explanation :: Maybe Text, -- Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters.
  poll_explanationEntities :: Maybe MessageEntity, -- Special entities like usernames, URLs, bot commands, etc. that appear in the explanation.
  poll_openPeriod :: Maybe Integer, --Amount of time in seconds the poll will be active after creation.
  poll_closeDate :: Maybe Integer -- Point in time (Unix timestamp) when the poll will be automatically closed.
  } deriving (Show,Generic) 

instance FromJSON Poll where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Poll where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | PollAnswer
data PollAnswer = PollAnswer {
  pollanswer_pollId :: Text, -- Unique poll identifier.
  pollanswer_user :: User, -- The user, who changed the answer to the poll.
  pollanswer_optionIds :: [Integer] -- 0-based identifiers of answer options, chosen by the user. May be empty if the user retracted their vote.
  } deriving (Show,Generic) 

instance FromJSON PollAnswer where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON PollAnswer where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | PollOption
data PollOption = PollOption {
  pollopt_text :: Text, -- Option text, 1-100 characters.
  pollopt_voterCount :: Integer -- Number of users that voted for this option.
  } deriving (Show,Generic) 

instance FromJSON PollOption where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON PollOption where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Game
data Game = Game {
  game_title :: Text, -- Title of the game
  game_description :: Text, -- Description of the game.
  game_photo :: [PhotoSize], -- Photo that will be displayed in the game message in chats.
  game_text :: Maybe Text, -- Brief description of the game or high scores included in the game message.
  game_textEntities :: Maybe [MessageEntity], -- Special entities that appear in text, such as usernames, URLs, bot commands, etc.
  game_animation :: Maybe Animation -- Animation that will be displayed in the game message in chats.
  } deriving (Show,Generic) 

instance FromJSON Game where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Game where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Dice
data Dice = Dice {
  dice_emoji :: Text, -- Emoji on which the dice throw animation is based.
  dice_value :: Integer -- Value of the dice.
  } deriving (Show,Generic) 

instance FromJSON Dice where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Dice where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Voice
data Voice = Voice {
  voice_fileId :: Text, -- Identifier for this file, which can be used to download or reuse the file.
  voice_fileUniqueId :: Text, -- Unique identifier for this file.
  voice_duration :: Integer, -- Duration of the audio in seconds as defined by sender.
  voice_mimeType :: Maybe Text, -- MIME type of the file as defined by sender.
  voice_fileSize :: Maybe Integer -- File size.
  } deriving (Show,Generic) 

instance FromJSON Voice where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Voice where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Document
data Document = Document {
  document_fileId :: Text, -- Unique identifier for this file.
  document_file_unique_id :: Text, -- Identifier for this file, which can be used to download or reuse the file.
  document_fileName :: Text, -- Original filename as defined by sender
  document_mimeType :: Maybe Text, -- MIME type of the file as defined by sender.
  document_fileSize :: Maybe Integer -- File size.
  } deriving (Show,Generic) 

instance FromJSON Document where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON Document where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | Location
data Location = Location {
  location_latitude :: Float, -- Longitude as defined by sender.
  location_longitude :: Float -- Latitude as defined by sender.
  } deriving (Show,Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON Location where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | Audio
data Audio = Audio {
  audio_fileId :: Text, -- Identifier for this file.
  audio_file_unique_id :: Text, -- Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  audio_duration :: Integer, -- Duration of the audio in seconds as defined by sender.
  audio_performer :: Maybe Text, -- Performer of the audio as defined by sender or by audio tags.
  audio_title :: Maybe Text, -- Title of the audio as defined by sender or by audio tags.
  audio_fileName :: Maybe Text, -- Original filename as defined by sender.
  audio_mimeType :: Text, -- MIME type of the file as defined by sender.
  audio_fileSize :: Integer -- File size.
  } deriving (Show,Generic)

instance FromJSON Audio where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Audio where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Contact
data Contact = Contact {
  contact_phoneNumber :: Text, -- Contact's phone number.
  contact_firstName :: Text, -- Contact's first name.
  contact_vcard :: Maybe Text -- Additional data about the contact in the form.
  } deriving (Show,Generic)

instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Contact where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Message
data Message = Message {
  message_messageId :: Integer, -- Unique message identifier inside this chat.
  message_from :: Maybe User, -- Sender, empty for messages sent to channels.
  message_senderChat :: Maybe Chat, -- Sender of the message, sent on behalf of a chat.
  message_date :: Integer, -- Date the message was sent in Unix time.
  message_chat :: Chat, -- Conversation the message belongs to.
  message_forwardFrom :: Maybe User, -- For forwarded messages, sender of the original message.
  message_forwardFromchat :: Maybe Chat, -- For messages forwarded from channels or from anonymous administrators, information about the original sender chat.
  message_forward_fromMessageId :: Maybe Integer, -- For messages forwarded from channels, identifier of the original message in the channel.
  message_forwardSignature :: Maybe Text, -- For messages forwarded from channels, signature of the post author if present.
  message_forwardSenderName :: Maybe Text, -- Sender's name for messages forwarded from users who disallow adding a link to their account in forwarded messages.
  message_forwardDate :: Maybe Integer, -- For forwarded messages, date the original message was sent in Unix time.
  message_replyToMessage :: Maybe Message, -- For replies, the original message.
  message_viaBot :: Maybe User, -- Bot through which the message was sent.
  message_editTime :: Maybe Integer, -- Date the message was last edited in Unix time.
  message_mediaGroupId :: Maybe Text, -- The unique identifier of a media message group this message belongs to.
  message_authorSignature :: Maybe Text, -- Signature of the post author for messages in channels, or the custom title of an anonymous group administrator.
  message_text :: Maybe Text, -- For text messages, the actual UTF-8 text of the message, 0-4096 characters.
  message_entities :: Maybe [MessageEntity], --  For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text.
  message_animation :: Maybe Animation, -- Message is an animation, information about the animation.
  message_audio :: Maybe Audio, -- Message is an audio file, information about the file.
  message_document :: Maybe Document, -- Message is a general file, information about the file.
  message_photo :: Maybe [PhotoSize], -- Message is a photo, available sizes of the photo.
  message_sticker :: Maybe Sticker, -- Message is a sticker, information about the sticker.
  message_video :: Maybe Video, -- Message is a video, information about the video.
  message_videoNote :: Maybe VideoNote, -- Message is a video note, information about the video message.
  message_voice :: Maybe Voice, -- Message is a voice message, information about the file.
  message_caption :: Maybe Text, -- Caption for the animation, audio, document, photo, video or voice, 0-1024 characters.
  message_captionEntities :: Maybe [MessageEntity], -- For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption.
  message_contact :: Maybe Contact, -- Message is a shared contact, information about the contact.
  message_dice :: Maybe Dice, -- Message is a dice with random value.
  message_game :: Maybe Game, -- Message is a game, information about the game.
  message_poll :: Maybe Poll, -- Message is a native poll, information about the poll.
  message_venue :: Maybe Venue, -- Message is a venue, information about the venue.
  message_location :: Maybe Location, -- Message is a shared location, information about the location.
  message_newChatMembers :: Maybe [User], -- New members that were added to the group or supergroup and information about them.
  message_leftChatMember :: Maybe User, -- A member was removed from the group, information about them.
  message_newChatTitle :: Maybe Text, -- A chat title was changed to this value.
  message_newChatPhoto :: Maybe [PhotoSize], -- A chat photo was change to this value.
  message_deleteChatPhoto :: Maybe Bool, -- Service message: the chat photo was deleted.
  message_groupChatCreated :: Maybe Bool, -- ervice message: the group has been created.
  message_supergroupChatCreated :: Maybe Bool, -- Service message: the supergroup has been created.
  message_channelChatCreated :: Maybe Bool, -- Service message: the channel has been created.
  message_messageAutoDeleteTimerChanged :: Maybe MessageAutoDeleteTimerChanged, -- ervice message: auto-delete timer settings changed in the chat.
  message_migrateToChatId :: Maybe Integer, -- The group has been migrated to a supergroup with the specified identifier.
  message_migrateFromChatId :: Maybe Integer, -- The supergroup has been migrated from a group with the specified identifier.
  message_pinnedMessage :: Maybe Message, -- Specified message was pinned.
  message_invoice :: Maybe Invoice, -- Message is an invoice for a payment.
  message_connectedWebsite :: Maybe Text -- he domain name of the website on which the user has logged in.
  } deriving (Show,Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | MessageEntity
data MessageEntity = MessageEntity {
  messageent_type :: Text, -- Type of the entity. Can be “mention” (@username), “hashtag” (#hashtag),...
  messageent_offset :: Integer, -- Offset in UTF-16 code units to the start of the entity.
  messageent_length :: Integer, -- Length of the entity in UTF-16 code units.
  messageent_url :: Maybe Text, --  For “text_link” only, url that will be opened after user taps on the text.
  messageent_user :: Maybe User, -- For “text_mention” only, the mentioned user.
  messageent_language :: Maybe Text -- For “pre” only, the programming language of the entity text.
  } deriving (Show,Generic)

instance FromJSON MessageEntity where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON MessageEntity where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

-- | MessageAutoDeleteTimerChanged
newtype MessageAutoDeleteTimerChanged = MessageAutoDeleteTimerChanged {
  messageautodelete_messageAutoDeleteTime :: Integer -- New auto-delete time for messages in the chat.
} deriving (Show,Generic)

instance FromJSON MessageAutoDeleteTimerChanged where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 18 }

instance ToJSON MessageAutoDeleteTimerChanged where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 18 }

-- | Invoice
data Invoice = Invoice {
  invoice_title :: Text, -- Product name.
  invoice_description :: Text, -- Product description.
  invoice_startParameter :: Text, -- Unique bot deep-linking parameter that can be used to generate this invoice.
  invoice_currency :: Text, -- Three-letter ISO 4217 currency code.
  invoice_totalAmount :: Integer -- Total price in the smallest units of the currency.
  } deriving (Show,Generic)

instance FromJSON Invoice where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Invoice where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Update
data Update = Update {
  update_updateId :: Integer, -- The update's unique identifier.
  update_message :: Maybe Message, -- New incoming message of any kind — text, photo, sticker, etc.
  update_editedMessage :: Maybe Message, -- New version of a message that is known to the bot and was edited.
  update_channelPost :: Maybe Message, -- New incoming channel post of any kind — text, photo, sticker, etc.
  update_editedChannelPost :: Maybe Message, -- New version of a channel post that is known to the bot and was edited.
  update_poll :: Maybe Poll, -- New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot.
  update_pollAnswer :: Maybe PollAnswer -- A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
} deriving (Show,Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Update where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | Status
type Status = Bool -- Responce status.

-- | UpdateData
data UpdateData = UpdateData {
  ok :: Status, -- Responce status.
  result :: [Update] -- Update's array.
} deriving (Show, Generic, FromJSON, ToJSON)