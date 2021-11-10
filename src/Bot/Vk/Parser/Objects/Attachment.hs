{-# LANGUAGE DeriveGeneric #-}

module Bot.Vk.Parser.Objects.Attachment where

import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Photo (Photo(..))
import Bot.Vk.Parser.Objects.Video (Video(..))
import Bot.Vk.Parser.Objects.Audio (Audio(..))
import Bot.Vk.Parser.Objects.Document (Document(..))
import Bot.Vk.Parser.Objects.Link (Link(..))
import Bot.Vk.Parser.Objects.Market (Market(..))
import Bot.Vk.Parser.Objects.MarketAlbum (MarketAlbum(..))
import Bot.Vk.Parser.Objects.Wall (Wall(..))
import Bot.Vk.Parser.Objects.WallReply (WallReply(..))
import Bot.Vk.Parser.Objects.Sticker (Sticker(..))
import Bot.Vk.Parser.Objects.Gift (Gift(..))

data Attachment = AttachPhoto Photo
                | AttachVideo Video
                | AttachAudio Audio
                | AttachDoc Document
                | AttachLink Link
                | AttachMarket Market
                | AttachMarketAlbum MarketAlbum
                | AttachWall Wall
                | AttachWallReply WallReply
                | AttachSticker Sticker
                | AttachGift Gift
                | AttachInvalid
                deriving (Show, Read, Eq, Generic)

instance FromJSON Attachment where
  parseJSON = A.withObject "Attachment"$ \o -> do
    attachType <- o A..: "type"
    case (attachType :: T.Text) of
      "photo" -> do
        photo <- o A..: "photo"
        AttachPhoto <$> (Photo
          <$> photo A..: "id"
          <*> photo A..: "owner_id"
          <*> photo A..: "access_key"
          )
      "video" -> do
        video <- o A..: "video"
        AttachVideo <$> (Video
          <$> video A..: "id"
          <*> video A..: "owner_id"
          <*> video A..: "access_key"
          )
      "audio" -> do
        audio <- o A..: "audio"
        AttachAudio <$> (Audio
          <$> audio A..: "id"
          <*> audio A..: "owner_id"
          )
      "doc" -> do
        doc <- o A..: "doc"
        AttachDoc <$> (Document
          <$> doc A..: "id"
          <*> doc A..: "owner_id"
          <*> doc A..: "title"
          <*> doc A..: "url"
          <*> doc A..: "access_key"
          )
      "link" -> do
        link <- o A..: "link"
        AttachLink <$> (Link
          <$> link A..: "url")
      "market_item" -> do
        marketItem <- o A..: "market_item"
        AttachMarket <$> (Market
          <$> marketItem A..: "id"
          <*> marketItem A..: "owner_id")
      "market_collection" -> do
        marketCollection <- o A..: "market_collection"
        AttachMarketAlbum <$> (MarketAlbum
          <$> marketCollection A..: "id"
          <*> marketCollection A..: "owner_id")
      "wall_post" -> do
        wallPost <- o A..: "wall_post"
        AttachWall <$> (Wall
          <$> wallPost A..: "id"
          <*> wallPost A..: "owner_id")
      "wall_comment" -> do
        wallComment <- o A..: "wall_comment"
        AttachWallReply <$> (WallReply
          <$> wallComment A..: "id"
          <*> wallComment A..: "owner_id")
      "sticker" -> do
        sticker <- o A..: "sticker"
        AttachSticker <$> (Sticker
          <$> sticker A..: "id")
      "gift" -> do
        gift <- o A..: "gift"
        AttachGift <$> (Gift
          <$> gift A..: "id")
      _ -> return AttachInvalid
