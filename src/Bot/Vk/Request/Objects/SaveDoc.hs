{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Request.Objects.SaveDoc where

import Web.FormUrlEncoded (ToForm(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Bot.Vk.Parser.Objects.Synonyms (Token, Version, AccessToken, FilePathT)

data SaveDoc = SaveDoc {
  file :: Text,
  access_token :: AccessToken,
  v :: Version
} deriving (Generic, ToForm)

-- | Default methods
saveNewDoc :: FilePathT -> Token -> Version -> SaveDoc
saveNewDoc fileName token vkVersion = SaveDoc {
  file = fileName,
  access_token = token,
  v = vkVersion
}