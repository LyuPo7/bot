{-# LANGUAGE DeriveGeneric, OverloadedLists #-}

module Bot.Vk.Request.Objects.GetUploadLink where

import qualified Web.HttpApiData as Url
import Web.FormUrlEncoded (ToForm(..))
import GHC.Generics (Generic)

import Bot.Vk.Parser.Objects.Synonyms (Token, Version, FileType,
                                       PeerId, AccessToken, Url)

data GetUploadLink = GetUploadLink {
  link_type :: Url,
  peer_id :: PeerId,
  access_token :: AccessToken,
  v :: Version
} deriving (Generic)

instance ToForm GetUploadLink where
  toForm upLink =
    [ ("type", Url.toQueryParam (link_type upLink)),
      ("peer_id", Url.toQueryParam (peer_id upLink)),
      ("access_token", Url.toQueryParam (access_token upLink)),
      ("v", Url.toQueryParam (v upLink))
       ]

-- | Default methods
getLink :: PeerId -> FileType -> Version -> Token -> GetUploadLink
getLink peerId fileType vkVersion token = GetUploadLink {
  peer_id = peerId,
  link_type = fileType,
  v = vkVersion,
  access_token = token
}