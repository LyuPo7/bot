{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.GetLongPollServer where

import Web.FormUrlEncoded (ToForm(..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data GetLongPollServer = GetLongPollServer {
  group_id :: BotSynonyms.GroupId,
  access_token :: BotSynonyms.Token,
  v :: BotSynonyms.Version
} deriving (Generic, ToForm)

getPollServer :: BotSynonyms.GroupId ->
                 BotSynonyms.Token ->
                 BotSynonyms.Version ->
                 GetLongPollServer
getPollServer groupId token vkVersion = GetLongPollServer { 
  group_id = groupId,
  access_token = token,
  v = vkVersion
}