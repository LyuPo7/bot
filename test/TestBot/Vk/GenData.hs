module TestBot.Vk.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Vk.Parser.Objects.UploadObjectResponse as UpObjResp
import qualified Bot.Vk.Parser.Objects.UploadFileResponse as UpFileResp
import qualified Bot.Vk.Parser.Objects.UploadUrlResponse as UpUrlResp
import qualified Bot.Vk.Parser.Objects.PollResponse as PollResp
import qualified Bot.Vk.Parser.Objects.UploadObject as UpObj
import qualified Bot.Vk.Parser.Objects.UploadUrl as UpUrl
import qualified Bot.Vk.Parser.Objects.Server as Server
import qualified Bot.Vk.Parser.Objects.UpdateData as UpData
import qualified Bot.Vk.Parser.Objects.Update as Update
import qualified Bot.Vk.Parser.Objects.Message as Message
import qualified Bot.Vk.Parser.Objects.Attachment as Attach
import qualified Bot.Vk.Parser.Objects.Gift as Gift
import qualified Bot.Vk.Parser.Objects.Sticker as Sticker
import qualified Bot.Vk.Parser.Objects.Document as Doc
import qualified Bot.Vk.Parser.Objects.Photo as Photo
import qualified Bot.Vk.Parser.Objects.Video as Video
import qualified Bot.Vk.Parser.Objects.Audio as Audio
import qualified Bot.Vk.Parser.Objects.WallReply as WallReply
import qualified Bot.Vk.Parser.Objects.Wall as Wall
import qualified Bot.Vk.Parser.Objects.Market as Market
import qualified Bot.Vk.Parser.Objects.MarketAlbum as MarketAlbum
import qualified Bot.Vk.Parser.Objects.Link as Link
import qualified Bot.Vk.Parser.Objects.Geo as Geo

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genGeo :: Gen Geo.Geo
genGeo = Geo.Geo
  <$> Gen.element ["111.345 56.789",
                   "99.999 109.107"]

genPhoto :: Gen Photo.Photo
genPhoto = Photo.Photo
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genVideo :: Gen Video.Video
genVideo = Video.Video
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genAudio :: Gen Audio.Audio
genAudio = Audio.Audio
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genDoc :: Gen Doc.Document
genDoc = Doc.Document
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["book.pdf",
                   "log.txt",
                   "prog.py"]
  <*> Gen.element ["https://server/link/123",
                   "https://server/link/222"]
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genLink :: Gen Link.Link
genLink = Link.Link
  <$> Gen.element ["https://server/link/123",
                   "https://server/link/222"]

genMarket :: Gen Market.Market
genMarket = Market.Market
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genMarketAlbum :: Gen MarketAlbum.MarketAlbum
genMarketAlbum = MarketAlbum.MarketAlbum
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genWall :: Gen Wall.Wall
genWall = Wall.Wall
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genWallReply :: Gen WallReply.WallReply
genWallReply = WallReply.WallReply
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genSticker :: Gen Sticker.Sticker
genSticker = Sticker.Sticker
  <$> (toInteger <$> genId)

genGift :: Gen Gift.Gift
genGift = Gift.Gift
  <$> (toInteger <$> genId)

genAttach :: Gen Attach.Attachment
genAttach = do
  photo <- genPhotoAttach
  video <- genVideoAttach
  audio <- genAudioAttach
  doc <- genDocAttach
  Gen.element [photo, video, audio, doc]

genCompAttach :: Gen Attach.Attachment
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

genNotDocAttach :: Gen Attach.Attachment
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

genNotStickerAttach :: Gen Attach.Attachment
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

genDocAttach :: Gen Attach.Attachment
genDocAttach = Attach.AttachDoc
  <$> genDoc

genGiftAttach :: Gen Attach.Attachment
genGiftAttach = Attach.AttachGift
  <$> genGift

genStickerAttach :: Gen Attach.Attachment
genStickerAttach = Attach.AttachSticker
  <$> genSticker

genWallReplyAttach :: Gen Attach.Attachment
genWallReplyAttach = Attach.AttachWallReply
  <$> genWallReply

genWallAttach :: Gen Attach.Attachment
genWallAttach = Attach.AttachWall
  <$> genWall

genMarketAlbumAttach :: Gen Attach.Attachment
genMarketAlbumAttach = Attach.AttachMarketAlbum
  <$> genMarketAlbum

genMarketAttach :: Gen Attach.Attachment
genMarketAttach = Attach.AttachMarket
  <$> genMarket

genLinkAttach :: Gen Attach.Attachment
genLinkAttach = Attach.AttachLink
  <$> genLink

genVideoAttach :: Gen Attach.Attachment
genVideoAttach = Attach.AttachVideo
  <$> genVideo

genAudioAttach :: Gen Attach.Attachment
genAudioAttach = Attach.AttachAudio
  <$> genAudio

genPhotoAttach :: Gen Attach.Attachment
genPhotoAttach = Attach.AttachPhoto
  <$> genPhoto

genMessage :: Gen Message.Message
genMessage = Message.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["Hi!", "Let's talk!", ""]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genNum5Message :: Gen Message.Message
genNum5Message = Message.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.constant "5"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genBotHelpMessage :: Gen Message.Message
genBotHelpMessage = Message.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.constant "/help"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genBotRepeatMessage :: Gen Message.Message
genBotRepeatMessage = Message.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.constant "/repeat"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genMessageWoBotCom :: Gen Message.Message
genMessageWoBotCom = Message.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["", "Hi!", "Let's talk!"]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genUpdate :: Gen Update.Update
genUpdate = Update.Update
  <$> Gen.element ["new_message", "reply_message"]
  <*> genMessage

genUpdateData :: Gen UpData.UpdateData
genUpdateData = UpData.UpdateData
  <$> Gen.text (Range.constant 10 15) Gen.digit
  <*> Gen.list (Range.constant 0 10) genUpdate

genServer :: Gen Server.ServerText
genServer = Server.ServerText
  <$> Gen.text (Range.constant 10 15) Gen.ascii
  <*> Gen.element ["https://server/server1",
                   "https://server/server2"]
  <*> Gen.text (Range.constant 10 15) Gen.digit

genPollResponse :: Gen PollResp.PollResponse
genPollResponse = PollResp.PollResponse
  <$> genServer

genUploadUrl :: Gen UpUrl.UploadUrl
genUploadUrl = UpUrl.UploadUrl
  <$> Gen.element ["https://server/link/123",
                   "https://server/link/222"]

genUploadUrlResponse :: Gen UpUrlResp.UploadUrlResponse
genUploadUrlResponse = UpUrlResp.UploadUrlResponse
  <$> Gen.maybe genUploadUrl

genUploadFileResponse :: Gen UpFileResp.UploadFileResponse
genUploadFileResponse = UpFileResp.UploadFileResponse
  <$> Gen.maybe (Gen.text (Range.constant 10 15) Gen.ascii)

genUploadObject :: Gen UpObj.UploadObject
genUploadObject = UpObj.UploadObject
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["https://server/obj/23",
                   "https://server/obj/77"]

genUploadObjectResponse :: Gen UpObjResp.UploadObjectResponse
genUploadObjectResponse = UpObjResp.UploadObjectResponse
  <$> Gen.list (Range.constant 0 10) genUploadObject