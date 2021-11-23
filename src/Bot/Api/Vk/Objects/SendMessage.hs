{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.SendMessage where

import Web.FormUrlEncoded (ToForm(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data SendMessage = SendMessage {
  access_token :: BotSynonyms.Token,
  user_id :: BotSynonyms.ChatId,
  message :: Text,
  v :: BotSynonyms.Version,
  attachment :: Maybe Text,
  sticker_id :: Maybe BotSynonyms.StickerId,
  lat :: Maybe Double,
  long :: Maybe Double
} deriving (Generic, ToForm)

defaultMessage :: BotSynonyms.ChatId ->
                  BotSynonyms.Token ->
                  BotSynonyms.Version ->
                  SendMessage
defaultMessage userId token vkVersion = SendMessage { 
  access_token = token,
  user_id = userId,
  message = "",
  v = vkVersion,
  attachment = Nothing,
  sticker_id = Nothing,
  lat = Nothing,
  long = Nothing
}