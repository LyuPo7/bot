{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Request.Objects.SendMessage where

import Web.FormUrlEncoded (ToForm(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Bot.Vk.Parser.Objects.Synonyms (Token, Version,
                                       UserId, AccessToken)

data SendMessage = SendMessage {
  access_token :: AccessToken,
  user_id :: UserId,
  message :: Text,
  v :: Text,
  attachment :: Maybe Text,
  sticker_id :: Maybe Integer,
  lat :: Maybe Double,
  long :: Maybe Double
} deriving (Generic, ToForm)

-- | Default methods
defaultMessage :: UserId -> Token -> Version -> SendMessage
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