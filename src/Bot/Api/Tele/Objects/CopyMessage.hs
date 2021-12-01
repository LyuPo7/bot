{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.CopyMessage where

import Data.Aeson.Types (ToJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data CopyMessage = CopyMessage
  { chat_id :: BotSynonyms.ChatId,
    from_chat_id :: BotSynonyms.ChatId,
    message_id :: BotSynonyms.MessageId
  }
  deriving (Show, Eq, Generic, ToJSON)

createEchoMessage ::
  BotSynonyms.ChatId ->
  BotSynonyms.MessageId ->
  CopyMessage
createEchoMessage chatId messageId =
  CopyMessage
    { chat_id = chatId,
      from_chat_id = chatId,
      message_id = messageId
    }
