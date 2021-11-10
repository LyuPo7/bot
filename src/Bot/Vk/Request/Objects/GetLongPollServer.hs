{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Request.Objects.GetLongPollServer where

import Web.FormUrlEncoded (ToForm(..))
import GHC.Generics (Generic)

import Bot.Vk.Parser.Objects.Synonyms (Token, Version,
                                       GroupId, Version, AccessToken)

data GetLongPollServer = GetLongPollServer {
  group_id :: GroupId,
  access_token :: AccessToken,
  v :: Version
} deriving (Generic, ToForm)

-- | Default methods
getPollServer :: GroupId -> Token -> Version -> GetLongPollServer
getPollServer groupId token vkVersion = GetLongPollServer { 
  group_id = groupId,
  access_token = token,
  v = vkVersion
}