{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Tele.Request.Data where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..),
                         genericToJSON, defaultOptions,
                         fieldLabelModifier, genericParseJSON)

-- | Types for RequestOptions requests
newtype TeleRequest = TeleRequest { getRequest :: Text }

-- | Tele Requests
getUpdates :: TeleRequest
getUpdates = TeleRequest "/getUpdates"

sendMessage :: TeleRequest
sendMessage = TeleRequest "/sendMessage"

copyMessage :: TeleRequest
copyMessage = TeleRequest "/copyMessage"

getBotCommands :: TeleRequest
getBotCommands = TeleRequest "/getMyCommands"

setBotCommands :: TeleRequest
setBotCommands = TeleRequest "/setMyCommands"

data RequestOptions = 
  GetUpdates {
    updates_offset :: Maybe Integer, -- Identifier of the first update to be returned. All previous updates will forgotten.
    updates_limit :: Maybe Integer, -- Limits the number of updates to be retrieved.
    updates_timeout :: Maybe Integer, -- Timeout in seconds for long polling.
    updates_allowedUpdates :: Maybe [Text] -- List of the update types you want your bot to receive. Specify [“message”, “edited_channel_post”, “callback_query”].
  } | 
  SendMessage {
    sendMes_chatId :: Integer, -- Unique identifier for the target chat or username of the target channel.
    sendMes_text :: Text, -- Text of the message to be sent, 1-4096 characters after entities parsing.
    sendMes_disableNotification :: Maybe Bool, -- Users will receive a notification with no sound.
    sendMes_replyToMessageId :: Maybe Integer -- If the message is a reply, ID of the original message.
  } |
  QueryMessage {
    sendQue_chatId :: Integer, -- Unique identifier for the target chat or username of the target channel.
    sendQue_text :: Text, -- Text of the message to be sent, 1-4096 characters after entities parsing.
    sendQue_disableNotification :: Maybe Bool, -- Users will receive a notification with no sound.
    sendQue_replyToMessageId :: Maybe Integer, -- If the message is a reply, ID of the original message
    sendQue_replyMarkup :: Maybe InlineKeyboardMarkup -- Additional interface options.
  } |
  CopyMessage {
    copwMes_chatId :: Integer, -- Unique identifier for the target chat or username of the target channel.
    copwMes_fromChatId :: Integer, -- Unique identifier for the chat where the original message was sent.
    copwMes_messageId :: Integer -- Message identifier in the chat specified in from_chat_id.
  } |
  GetBotCommands {
    getComm_scope :: Maybe BotCommandScope, --A JSON-serialized object, describing scope of users.
    getComm_languageCode :: Maybe Text -- A two-letter ISO 639-1 language code or an empty string.
  } |
  SetBotCommands {
    setComm_commands :: [BotCommand], -- A JSON-serialized list of bot commands to be set as the list of the bot's commands.
    setComm_scope :: Maybe BotCommandScope, -- A JSON-serialized object, describing scope of users for which the commands are relevant.
    setComm_languageCode :: Maybe Text -- A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands.
  } deriving (Show,Generic)

instance FromJSON RequestOptions where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON RequestOptions where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | BotCommandScope
newtype BotCommandScope = BotCommandScopeDefault {
  scopeDefault_type :: Text -- Scope type, must be default.
} deriving (Show, Generic)

instance FromJSON BotCommandScope where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 13 }

instance ToJSON BotCommandScope where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 13 }

-- | BotCommand
data BotCommand = BotCommand {
  botCom_command :: Text, -- Text of the command, 1-32 characters. Can contain only lowercase English letters, digits and underscores.
  botCom_description :: Text -- Description of the command, 3-256 characters.
} deriving (Show, Generic)

instance FromJSON BotCommand where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 7 }

instance ToJSON BotCommand where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 7 }

-- | InlineKeyboardMarkup
data InlineKeyboardMarkup = InlineKeyboardMarkup {
  inlineMarkup_keyboard :: [[InlineKeyboardButton]], -- Array of button rows, each represented by an Array of InlineKeyboardButton objects.
  inlineMarkup_resizeKeyboard :: Maybe Bool, -- Requests clients to resize the keyboard vertically for optimal fit
  inlineMarkup_oneTimeKeyboard :: Maybe Bool -- Requests clients to hide the keyboard as soon as it's been used.
} deriving (Show, Generic)

instance FromJSON InlineKeyboardMarkup where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 13 }

instance ToJSON InlineKeyboardMarkup where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 13 }

-- | InlineKeyboardButton
data InlineKeyboardButton = InlineKeyboardButton {
  inlineButton_text :: Text, -- Label text on the button.
  inlineButton_callbackData :: Maybe Text -- Data to be sent in a callback query to the bot when button is pressed.
} deriving (Show, Generic)

instance FromJSON InlineKeyboardButton where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 13 }

instance ToJSON InlineKeyboardButton where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 13 }

-- | Default methods for Requests
createGetUpdates :: Maybe Integer -> RequestOptions
createGetUpdates updateId = GetUpdates {
  updates_offset = updateId,
  updates_limit = Nothing,
  updates_timeout = Nothing,
  updates_allowedUpdates = Just ["message"]
}

createSendMessage :: Integer -> Text -> RequestOptions
createSendMessage chatId text = SendMessage {
  sendMes_chatId = chatId,
  sendMes_text = text,
  sendMes_disableNotification = Nothing,
  sendMes_replyToMessageId = Nothing
}

createCopyMessage :: Integer -> Integer -> RequestOptions
createCopyMessage chatId messageId = CopyMessage { 
  copwMes_chatId = chatId,
  copwMes_fromChatId = chatId,
  copwMes_messageId = messageId
}

createQueryMessage :: Integer -> Text -> InlineKeyboardMarkup -> RequestOptions
createQueryMessage chatId question markupIn = QueryMessage {
  sendQue_chatId = chatId,
  sendQue_text = question,
  sendQue_disableNotification = Nothing,
  sendQue_replyToMessageId = Nothing,
  sendQue_replyMarkup = Just markupIn
}

createButton :: Text -> Text -> InlineKeyboardButton
createButton text callback = InlineKeyboardButton {
  inlineButton_text = text,
  inlineButton_callbackData = Just callback
}

createKeyboard :: [[InlineKeyboardButton]] -> InlineKeyboardMarkup
createKeyboard buttons = InlineKeyboardMarkup {
  inlineMarkup_keyboard = buttons,
  inlineMarkup_oneTimeKeyboard = Just True,
  inlineMarkup_resizeKeyboard = Just True
}

helpBotCommand :: BotCommand
helpBotCommand = BotCommand {
  botCom_command = "/help",
  botCom_description = "Info message"
}

repBotCommand :: BotCommand
repBotCommand = BotCommand {
  botCom_command = "/repeat",
  botCom_description = "Repeat settings"
}

defaultBotCommandScope :: BotCommandScope
defaultBotCommandScope = BotCommandScopeDefault {
  scopeDefault_type = "default"
}

createBotCommands :: RequestOptions
createBotCommands = SetBotCommands {
  setComm_commands = [helpBotCommand, repBotCommand],
  setComm_scope = Just defaultBotCommandScope,
  setComm_languageCode = Just ""
}
