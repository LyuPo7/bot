{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Message where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Api.Vk.Objects.Attachment as VkAttachment
import qualified Bot.Api.Vk.Objects.Geo as VkGeo
import qualified Bot.Objects.Synonyms as BotSynonyms

data Message = Message
  { message_id :: Maybe BotSynonyms.MessageId,
    user_id :: BotSynonyms.ChatId,
    body :: Text,
    geo :: Maybe VkGeo.Geo,
    attachments :: Maybe [VkAttachment.Attachment],
    fwd_messages :: Maybe [Message]
  }
  deriving (Show, Read, Eq, Generic, FromJSON)
