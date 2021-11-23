module TestBot.Api.Vk.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Api.Vk.Objects.UploadObject as VkUpObj
import qualified Bot.Api.Vk.Objects.UploadUrl as VkUpUrl
import qualified Bot.Api.Vk.Objects.Server as VkServer
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData
import qualified Bot.Api.Vk.Objects.Update as VkUpdate
import qualified Bot.Api.Vk.Objects.Message as VkMessage
import qualified Bot.Api.Vk.Objects.Attachment as VkAttach
import qualified Bot.Api.Vk.Objects.Gift as VkGift
import qualified Bot.Api.Vk.Objects.Sticker as VkSticker
import qualified Bot.Api.Vk.Objects.Document as VkDoc
import qualified Bot.Api.Vk.Objects.Photo as VkPhoto
import qualified Bot.Api.Vk.Objects.Video as VkVideo
import qualified Bot.Api.Vk.Objects.Audio as VkAudio
import qualified Bot.Api.Vk.Objects.WallReply as VkWallReply
import qualified Bot.Api.Vk.Objects.Wall as VkWall
import qualified Bot.Api.Vk.Objects.Market as VkMarket
import qualified Bot.Api.Vk.Objects.MarketAlbum as VkMarketAlbum
import qualified Bot.Api.Vk.Objects.Link as VkLink
import qualified Bot.Api.Vk.Objects.Geo as VkGeo

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genGeo :: Gen VkGeo.Geo
genGeo = VkGeo.Geo
  <$> Gen.element ["111.345 56.789",
                   "99.999 109.107"]

genPhoto :: Gen VkPhoto.Photo
genPhoto = VkPhoto.Photo
  <$> (BotSynonyms.PhotoId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))
  <*> (BotSynonyms.AccessKey <$> Gen.text (Range.constant 10 15) Gen.ascii)

genVideo :: Gen VkVideo.Video
genVideo = VkVideo.Video
  <$> (BotSynonyms.VideoId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))
  <*> (BotSynonyms.AccessKey <$> Gen.text (Range.constant 10 15) Gen.ascii)

genAudio :: Gen VkAudio.Audio
genAudio = VkAudio.Audio
  <$> (BotSynonyms.AudioId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))

genDoc :: Gen VkDoc.Document
genDoc = VkDoc.Document
  <$> (BotSynonyms.DocId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))
  <*> Gen.element ["book.pdf",
                   "log.txt",
                   "prog.py"]
  <*> (BotSynonyms.Url <$> Gen.element ["https://server/link/123",
                                        "https://server/link/222"])
  <*> (BotSynonyms.AccessKey <$> Gen.text (Range.constant 10 15) Gen.ascii)

genLink :: Gen VkLink.Link
genLink = VkLink.Link
  <$> (BotSynonyms.Url <$> Gen.element ["https://server/link/123",
                                        "https://server/link/222"])

genMarket :: Gen VkMarket.Market
genMarket = VkMarket.Market
  <$> (BotSynonyms.MarketId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))

genMarketAlbum :: Gen VkMarketAlbum.MarketAlbum
genMarketAlbum = VkMarketAlbum.MarketAlbum
  <$> (BotSynonyms.MarketAlbumId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))

genWall :: Gen VkWall.Wall
genWall = VkWall.Wall
  <$> (BotSynonyms.WallId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))

genWallReply :: Gen VkWallReply.WallReply
genWallReply = VkWallReply.WallReply
  <$> (BotSynonyms.WallReplyId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))

genSticker :: Gen VkSticker.Sticker
genSticker = VkSticker.Sticker
  <$> (BotSynonyms.StickerId <$> (toInteger <$> genId))

genGift :: Gen VkGift.Gift
genGift = VkGift.Gift
  <$> (BotSynonyms.GiftId <$> (toInteger <$> genId))

genAttach :: Gen VkAttach.Attachment
genAttach = do
  photo <- genPhotoAttach
  video <- genVideoAttach
  audio <- genAudioAttach
  doc <- genDocAttach
  Gen.element [photo, video, audio, doc]

genCompAttach :: Gen VkAttach.Attachment
genCompAttach = do
  link <- genLinkAttach
  market <- genMarketAttach
  marketAlbum <- genMarketAlbumAttach
  wall <- genWallAttach
  wallReply <- genWallReplyAttach
  sticker <- genStickerAttach
  gift <- genGiftAttach
  Gen.element [link, market, marketAlbum,
               wall, wallReply, sticker, gift]

genNotDocAttach :: Gen VkAttach.Attachment
genNotDocAttach = do
  photo <- genPhotoAttach
  video <- genVideoAttach
  audio <- genAudioAttach
  link <- genLinkAttach
  market <- genMarketAttach
  marketAlbum <- genMarketAlbumAttach
  wall <- genWallAttach
  wallReply <- genWallReplyAttach
  sticker <- genStickerAttach
  gift <- genGiftAttach
  Gen.element [photo, video, audio, link, market,
               marketAlbum, wall, wallReply, sticker, gift]

genNotStickerAttach :: Gen VkAttach.Attachment
genNotStickerAttach = do
  photo <- genPhotoAttach
  video <- genVideoAttach
  audio <- genAudioAttach
  link <- genLinkAttach
  market <- genMarketAttach
  marketAlbum <- genMarketAlbumAttach
  wall <- genWallAttach
  wallReply <- genWallReplyAttach
  doc <- genDocAttach
  gift <- genGiftAttach
  Gen.element [photo, video, audio, link, market,
               marketAlbum, wall, wallReply, doc, gift]

genDocAttach :: Gen VkAttach.Attachment
genDocAttach = VkAttach.AttachDoc
  <$> genDoc

genGiftAttach :: Gen VkAttach.Attachment
genGiftAttach = VkAttach.AttachGift
  <$> genGift

genStickerAttach :: Gen VkAttach.Attachment
genStickerAttach = VkAttach.AttachSticker
  <$> genSticker

genWallReplyAttach :: Gen VkAttach.Attachment
genWallReplyAttach = VkAttach.AttachWallReply
  <$> genWallReply

genWallAttach :: Gen VkAttach.Attachment
genWallAttach = VkAttach.AttachWall
  <$> genWall

genMarketAlbumAttach :: Gen VkAttach.Attachment
genMarketAlbumAttach = VkAttach.AttachMarketAlbum
  <$> genMarketAlbum

genMarketAttach :: Gen VkAttach.Attachment
genMarketAttach = VkAttach.AttachMarket
  <$> genMarket

genLinkAttach :: Gen VkAttach.Attachment
genLinkAttach = VkAttach.AttachLink
  <$> genLink

genVideoAttach :: Gen VkAttach.Attachment
genVideoAttach = VkAttach.AttachVideo
  <$> genVideo

genAudioAttach :: Gen VkAttach.Attachment
genAudioAttach = VkAttach.AttachAudio
  <$> genAudio

genPhotoAttach :: Gen VkAttach.Attachment
genPhotoAttach = VkAttach.AttachPhoto
  <$> genPhoto

genMessage :: Gen VkMessage.Message
genMessage = VkMessage.Message
  <$> Gen.maybe (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> (BotSynonyms.ChatId <$> (toInteger <$> genId))
  <*> Gen.element ["Hi!", "Let's talk!", ""]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genNum5Message :: Gen VkMessage.Message
genNum5Message = VkMessage.Message
  <$> Gen.maybe (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> (BotSynonyms.ChatId <$> (toInteger <$> genId))
  <*> Gen.constant "5"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genBotHelpMessage :: Gen VkMessage.Message
genBotHelpMessage = VkMessage.Message
  <$> Gen.maybe (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> (BotSynonyms.ChatId <$> (toInteger <$> genId))
  <*> Gen.constant "/help"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genBotRepeatMessage :: Gen VkMessage.Message
genBotRepeatMessage = VkMessage.Message
  <$> Gen.maybe (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> (BotSynonyms.ChatId <$> (toInteger <$> genId))
  <*> Gen.constant "/repeat"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genMessageWoBotCom :: Gen VkMessage.Message
genMessageWoBotCom = VkMessage.Message
  <$> Gen.maybe (BotSynonyms.MessageId <$> (toInteger <$> genId))
  <*> (BotSynonyms.ChatId <$> (toInteger <$> genId))
  <*> Gen.element ["", "Hi!", "Let's talk!"]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genUpdate :: Gen VkUpdate.Update
genUpdate = VkUpdate.Update
  <$> Gen.element ["new_message", "reply_message"]
  <*> genMessage

genUpdateData :: Gen VkUpData.UpdateData
genUpdateData = VkUpData.UpdateData
  <$> Gen.text (Range.constant 10 15) Gen.digit
  <*> Gen.list (Range.constant 0 10) genUpdate

genServer :: Gen VkServer.Server
genServer = VkServer.Server
  <$> Gen.text (Range.constant 10 15) Gen.ascii
  <*> Gen.element ["https://server/server1",
                   "https://server/server2"]
  <*> (toInteger <$> genId)

genServerText :: Gen VkServer.ServerText
genServerText = VkServer.ServerText
  <$> Gen.text (Range.constant 10 15) Gen.ascii
  <*> Gen.element ["https://server/server1",
                   "https://server/server2"]
  <*> Gen.text (Range.constant 10 15) Gen.digit

genVkPollResponse :: Gen VkPollResp.PollResponse
genVkPollResponse = VkPollResp.PollResponse
  <$> genServerText

genUploadUrl :: Gen VkUpUrl.UploadUrl
genUploadUrl = VkUpUrl.UploadUrl
  <$> (BotSynonyms.Url <$> Gen.element ["https://server/link/123",
                                        "https://server/link/222"])

genUploadUrlResponse :: Gen VkUpUrlResp.UploadUrlResponse
genUploadUrlResponse = VkUpUrlResp.UploadUrlResponse
  <$> Gen.maybe genUploadUrl

genUploadFileResponse :: Gen VkUpFileResp.UploadFileResponse
genUploadFileResponse = VkUpFileResp.UploadFileResponse
  <$> Gen.maybe (Gen.text (Range.constant 10 15) Gen.ascii)

genUploadObject :: Gen VkUpObj.UploadObject
genUploadObject = VkUpObj.UploadObject
  <$> (BotSynonyms.ObjectId <$> (toInteger <$> genId))
  <*> (BotSynonyms.UserId <$> (toInteger <$> genId))
  <*> (BotSynonyms.Url <$> Gen.element ["https://server/link/123",
                                        "https://server/link/222"])

genUploadObjectResponse :: Gen VkUpObjResp.UploadObjectResponse
genUploadObjectResponse = VkUpObjResp.UploadObjectResponse
  <$> Gen.list (Range.constant 0 10) genUploadObject