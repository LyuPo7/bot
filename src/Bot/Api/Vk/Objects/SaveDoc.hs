{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.SaveDoc where

import GHC.Generics (Generic)
import Web.FormUrlEncoded (ToForm (..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data SaveDoc = SaveDoc
  { file :: BotSynonyms.FilePathT,
    access_token :: BotSynonyms.Token,
    v :: BotSynonyms.Version
  }
  deriving (Generic, ToForm)

saveNewDoc ::
  BotSynonyms.FilePathT ->
  BotSynonyms.Token ->
  BotSynonyms.Version ->
  SaveDoc
saveNewDoc fileName token vkVersion =
  SaveDoc
    { file = fileName,
      access_token = token,
      v = vkVersion
    }
