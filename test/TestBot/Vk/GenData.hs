module TestBot.Vk.GenData where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bot.Vk.Parser.Data as PD

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genUser :: Gen PD.Users
genUser = PD.Users
  <$> (toInteger <$> genId)

genGeo :: Gen PD.Geo
genGeo = PD.Geo
  <$> Gen.element ["111.345 56.789",
                   "99.999 109.107"]

genPhoto :: Gen PD.Photo
genPhoto = PD.Photo
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genVideo :: Gen PD.Video
genVideo = PD.Video
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genAudio :: Gen PD.Audio
genAudio = PD.Audio
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genDoc :: Gen PD.Document
genDoc = PD.Document
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["book.pdf",
                   "log.txt",
                   "prog.py"]
  <*> Gen.element ["https://server/link/123",
                   "https://server/link/222"]
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genLink :: Gen PD.Link
genLink = PD.Link
  <$> Gen.element ["https://server/link/123",
                   "https://server/link/222"]

genMarket :: Gen PD.Market
genMarket = PD.Market
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genMarketAlbum :: Gen PD.MarketAlbum
genMarketAlbum = PD.MarketAlbum
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genWall :: Gen PD.Wall
genWall = PD.Wall
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genWallReply :: Gen PD.WallReply
genWallReply = PD.WallReply
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genSticker :: Gen PD.Sticker
genSticker = PD.Sticker
  <$> (toInteger <$> genId)

genGift :: Gen PD.Gift
genGift = PD.Gift
  <$> (toInteger <$> genId)

genAttach :: Gen PD.Attachment
genAttach = PD.Attachment
  <$> Gen.element ["doc",
                   "audio",
                   "video",
                   "photo"]
  <*> Gen.maybe genPhoto
  <*> Gen.maybe genVideo
  <*> Gen.maybe genAudio
  <*> Gen.maybe genDoc
  <*> Gen.maybe genLink
  <*> Gen.maybe genMarket
  <*> Gen.maybe genMarketAlbum
  <*> Gen.maybe genWall
  <*> Gen.maybe genWallReply
  <*> Gen.maybe genSticker
  <*> Gen.maybe genGift

genCompAttach :: Gen PD.Attachment
genCompAttach = PD.Attachment
  <$> Gen.element ["link",
                   "market",
                   "market_album",
                   "wall",
                   "wall_reply",
                   "sticker",
                   "gift"]
  <*> Gen.maybe genPhoto
  <*> Gen.maybe genVideo
  <*> Gen.maybe genAudio
  <*> Gen.maybe genDoc
  <*> Gen.maybe genLink
  <*> Gen.maybe genMarket
  <*> Gen.maybe genMarketAlbum
  <*> Gen.maybe genWall
  <*> Gen.maybe genWallReply
  <*> Gen.maybe genSticker
  <*> Gen.maybe genGift

genNotDocAttach :: Gen PD.Attachment
genNotDocAttach = PD.Attachment
  <$> Gen.element ["photo",
                   "audio",
                   "video",
                   "link",
                   "market",
                   "market_album",
                   "wall",
                   "wall_reply",
                   "sticker",
                   "gift"]
  <*> Gen.maybe genPhoto
  <*> Gen.maybe genVideo
  <*> Gen.maybe genAudio
  <*> Gen.maybe genDoc
  <*> Gen.maybe genLink
  <*> Gen.maybe genMarket
  <*> Gen.maybe genMarketAlbum
  <*> Gen.maybe genWall
  <*> Gen.maybe genWallReply
  <*> Gen.maybe genSticker
  <*> Gen.maybe genGift

genNotStickerAttach :: Gen PD.Attachment
genNotStickerAttach = PD.Attachment
  <$> Gen.element ["photo",
                   "audio",
                   "video",
                   "link",
                   "market",
                   "market_album",
                   "wall",
                   "wall_reply",
                   "doc",
                   "gift"]
  <*> Gen.maybe genPhoto
  <*> Gen.maybe genVideo
  <*> Gen.maybe genAudio
  <*> Gen.maybe genDoc
  <*> Gen.maybe genLink
  <*> Gen.maybe genMarket
  <*> Gen.maybe genMarketAlbum
  <*> Gen.maybe genWall
  <*> Gen.maybe genWallReply
  <*> Gen.maybe genSticker
  <*> Gen.maybe genGift

genMessage :: Gen PD.Message
genMessage = PD.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["Hi!", "Let's talk!", ""]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genNum5Message :: Gen PD.Message
genNum5Message = PD.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.constant "5"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genBotHelpMessage :: Gen PD.Message
genBotHelpMessage = PD.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.constant "/help"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genBotRepeatMessage :: Gen PD.Message
genBotRepeatMessage = PD.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.constant "/repeat"
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genMessageWoBotCom :: Gen PD.Message
genMessageWoBotCom = PD.Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["", "Hi!", "Let's talk!"]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genUpdate :: Gen PD.Update
genUpdate = PD.Update
  <$> Gen.element ["new_message", "reply_message"]
  <*> genMessage

genUpdateData :: Gen PD.UpdateData
genUpdateData = PD.UpdateData
  <$> Gen.text (Range.constant 10 15) Gen.digit
  <*> Gen.list (Range.constant 0 10) genUpdate

genServer :: Gen PD.ServerText
genServer = PD.ServerText
  <$> Gen.text (Range.constant 10 15) Gen.ascii
  <*> Gen.element ["https://server/server1",
                   "https://server/server2"]
  <*> Gen.text (Range.constant 10 15) Gen.digit

genPollResponse :: Gen PD.PollResponse
genPollResponse = PD.PollResponse
  <$> genServer

genUploadUrl :: Gen PD.UploadUrl
genUploadUrl = PD.UploadUrl
  <$> Gen.element ["https://server/link/123",
                   "https://server/link/222"]

genUploadUrlResponse :: Gen PD.UploadUrlResponse
genUploadUrlResponse = PD.UploadUrlResponse
  <$> Gen.maybe genUploadUrl

genUploadFileResponse :: Gen PD.UploadFileResponse
genUploadFileResponse = PD.UploadFileResponse
  <$> Gen.maybe (Gen.text (Range.constant 10 15) Gen.ascii)

genUploadObject :: Gen PD.UploadObject
genUploadObject = PD.UploadObject
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["https://server/obj/23",
                   "https://server/obj/77"]

genUploadObjectResponse :: Gen PD.UploadObjectResponse
genUploadObjectResponse = PD.UploadObjectResponse
  <$> Gen.list (Range.constant 0 10) genUploadObject