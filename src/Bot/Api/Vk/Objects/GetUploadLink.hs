{-# LANGUAGE DeriveGeneric, OverloadedLists #-}

module Bot.Api.Vk.Objects.GetUploadLink where

import qualified Web.HttpApiData as Url
import Web.FormUrlEncoded (ToForm(..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data GetUploadLink = GetUploadLink {
  link_type :: BotSynonyms.FileType,
  peer_id :: BotSynonyms.UserId,
  access_token :: BotSynonyms.Token,
  v :: BotSynonyms.Version
} deriving (Generic)

instance ToForm GetUploadLink where
  toForm upLink =
    [ ("type", Url.toQueryParam (link_type upLink)),
      ("peer_id", Url.toQueryParam (peer_id upLink)),
      ("access_token", Url.toQueryParam (access_token upLink)),
      ("v", Url.toQueryParam (v upLink))
       ]

getLink :: BotSynonyms.UserId ->
           BotSynonyms.FileType ->
           BotSynonyms.Version ->
           BotSynonyms.Token ->
           GetUploadLink
getLink peerId fileType vkVersion token = GetUploadLink {
  peer_id = peerId,
  link_type = fileType,
  v = vkVersion,
  access_token = token
}