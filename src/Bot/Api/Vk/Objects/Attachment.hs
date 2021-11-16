{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Attachment where

import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Api.Vk.Objects.Photo as VkPhoto
import qualified Bot.Api.Vk.Objects.Video as VkVideo
import qualified Bot.Api.Vk.Objects.Audio as VkAudio
import qualified Bot.Api.Vk.Objects.Document as VkDoc
import qualified Bot.Api.Vk.Objects.Link as VkLink
import qualified Bot.Api.Vk.Objects.Market as VkMarket
import qualified Bot.Api.Vk.Objects.MarketAlbum as VkMarketAlbum
import qualified Bot.Api.Vk.Objects.Wall as VkWall
import qualified Bot.Api.Vk.Objects.WallReply as VkWallReply
import qualified Bot.Api.Vk.Objects.Sticker as VkSticker
import qualified Bot.Api.Vk.Objects.Gift as VkGift

data Attachment = AttachPhoto VkPhoto.Photo
                | AttachVideo VkVideo.Video
                | AttachAudio VkAudio.Audio
                | AttachDoc VkDoc.Document
                | AttachLink VkLink.Link
                | AttachMarket VkMarket.Market
                | AttachMarketAlbum VkMarketAlbum.MarketAlbum
                | AttachWall VkWall.Wall
                | AttachWallReply VkWallReply.WallReply
                | AttachSticker VkSticker.Sticker
                | AttachGift VkGift.Gift
                | AttachInvalid
                deriving (Show, Read, Eq, Generic)

instance FromJSON Attachment where
  parseJSON = A.withObject "Attachment"$ \o -> do
    attachType <- o A..: "type"
    case (attachType :: T.Text) of
      "photo" -> do
        photo <- o A..: "photo"
        AttachPhoto <$> (VkPhoto.Photo
          <$> photo A..: "id"
          <*> photo A..: "owner_id"
          <*> photo A..: "access_key"
          )
      "video" -> do
        video <- o A..: "video"
        AttachVideo <$> (VkVideo.Video
          <$> video A..: "id"
          <*> video A..: "owner_id"
          <*> video A..: "access_key"
          )
      "audio" -> do
        audio <- o A..: "audio"
        AttachAudio <$> (VkAudio.Audio
          <$> audio A..: "id"
          <*> audio A..: "owner_id"
          )
      "doc" -> do
        doc <- o A..: "doc"
        AttachDoc <$> (VkDoc.Document
          <$> doc A..: "id"
          <*> doc A..: "owner_id"
          <*> doc A..: "title"
          <*> doc A..: "url"
          <*> doc A..: "access_key"
          )
      "link" -> do
        link <- o A..: "link"
        AttachLink <$> (VkLink.Link
          <$> link A..: "url")
      "market_item" -> do
        marketItem <- o A..: "market_item"
        AttachMarket <$> (VkMarket.Market
          <$> marketItem A..: "id"
          <*> marketItem A..: "owner_id")
      "market_collection" -> do
        marketCollection <- o A..: "market_collection"
        AttachMarketAlbum <$> (VkMarketAlbum.MarketAlbum
          <$> marketCollection A..: "id"
          <*> marketCollection A..: "owner_id")
      "wall_post" -> do
        wallPost <- o A..: "wall_post"
        AttachWall <$> (VkWall.Wall
          <$> wallPost A..: "id"
          <*> wallPost A..: "owner_id")
      "wall_comment" -> do
        wallComment <- o A..: "wall_comment"
        AttachWallReply <$> (VkWallReply.WallReply
          <$> wallComment A..: "id"
          <*> wallComment A..: "owner_id")
      "sticker" -> do
        sticker <- o A..: "sticker"
        AttachSticker <$> (VkSticker.Sticker
          <$> sticker A..: "id")
      "gift" -> do
        gift <- o A..: "gift"
        AttachGift <$> (VkGift.Gift
          <$> gift A..: "id")
      _ -> return AttachInvalid
