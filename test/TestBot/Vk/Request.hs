{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Request where

import Control.Monad.Identity

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec ()
import Test.Hspec

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Request.AttachSpec as Attach
import qualified Bot.Vk.Request.Requests as Req
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import Bot.Vk.Parser.Data

genId :: Gen Int
genId = Gen.int (Range.constant 0 1000)

genUser :: Gen Users
genUser = Users
  <$> (toInteger <$> genId)

genGeo :: Gen Geo
genGeo = Geo
  <$> Gen.element ["111.345 56.789", "99.999 109.107"]

genPhoto :: Gen Photo
genPhoto = Photo
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genVideo :: Gen Video
genVideo = Video
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genAudio :: Gen Audio
genAudio = Audio
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genDoc :: Gen Document
genDoc = Document
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["book.pdf", "log.txt", "prog.py"]
  <*> Gen.element ["https://server/link/123", "https://server/link/222"]
  <*> Gen.text (Range.constant 10 15) Gen.ascii

genLink :: Gen Link
genLink = Link
  <$> Gen.element ["https://server/link/123", "https://server/link/222"]

genMarket :: Gen Market
genMarket = Market
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genMarketAlbum :: Gen MarketAlbum
genMarketAlbum = MarketAlbum
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genWall :: Gen Wall
genWall = Wall
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genWallReply :: Gen WallReply
genWallReply = WallReply
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)

genSticker :: Gen Sticker
genSticker = Sticker
  <$> (toInteger <$> genId)

genGift :: Gen Gift
genGift = Gift
  <$> (toInteger <$> genId)

genAttach :: Gen Attachment
genAttach = Attachment
  <$> Gen.element ["doc", "audio", "video", "photo"]
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

genCompAttach :: Gen Attachment
genCompAttach = Attachment
  <$> Gen.element ["link", "market", "market_album", "wall", "wall_reply", "sticker", "gift"]
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

genNotStickerAttach :: Gen Attachment
genNotStickerAttach = Attachment
  <$> Gen.element ["photo", "audio", "video", "link", "market", "market_album", "wall", "wall_reply", "doc", "gift"]
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

genMessage :: Gen Message
genMessage = Message
  <$> Gen.maybe (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["Hi!", "Let's talk!", ""]
  <*> Gen.maybe genGeo
  <*> Gen.maybe (Gen.list (Range.constant 0 10) genAttach)
  <*> Gen.constant Nothing

genUpdate :: Gen Update
genUpdate = Update
  <$> Gen.element ["new_message", "reply_message"]
  <*> genMessage

genUpdateData :: Gen UpdateData
genUpdateData = UpdateData
  <$> Gen.text (Range.constant 10 15) Gen.digit
  <*> Gen.list (Range.constant 0 10) genUpdate

genServer :: Gen Server
genServer = Server
  <$> Gen.text (Range.constant 10 15) Gen.ascii
  <*> Gen.element ["https://server/server1", "https://server/server2"]
  <*> (toInteger <$> genId)

genPollResponse :: Gen PollResponse
genPollResponse = PollResponse
  <$> genServer

genUploadUrl :: Gen UploadUrl
genUploadUrl = UploadUrl
  <$> Gen.element ["https://server/link/123", "https://server/link/222"]

genUploadUrlResponse :: Gen UploadUrlResponse
genUploadUrlResponse = UploadUrlResponse
  <$> Gen.maybe genUploadUrl

genUploadFileResponse :: Gen UploadFileResponse
genUploadFileResponse = UploadFileResponse
  <$> Gen.maybe (Gen.text (Range.constant 10 15) Gen.ascii)

genUploadObject :: Gen UploadObject
genUploadObject = UploadObject
  <$> (toInteger <$> genId)
  <*> (toInteger <$> genId)
  <*> Gen.element ["https://server/obj/23", "https://server/obj/77"]

genUploadObjectResponse :: Gen UploadObjectResponse
genUploadObjectResponse = UploadObjectResponse
  <$> Gen.list (Range.constant 0 10) genUploadObject

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}

reqH :: ReqSpec.Handle Identity
reqH = ReqSpec.Handle {
    ReqSpec.hLogger = logH,
    ReqSpec.configReq = runC,
    ReqSpec.hParser = parserH
}

runC :: Settings.Config
runC = Settings.Config {
    Settings.botApi = "telegram",
    Settings.botToken = "abcd0dcba",
    Settings.botInitialReplyNumber = 3,
    Settings.botQuestion = "How many replies do you prefer to recieve?",
    Settings.botDescription = "Hi! I'm bot=)",
    Settings.botGroupId = Nothing
}

handle :: Attach.Handle Identity
handle = Attach.Handle {
    Attach.hLogger = logH,
    Attach.hReq = reqH,
    Attach.hParser = parserH,

    Attach.updateDoc = \doc -> Identity doc
}

handleDoc :: Attach.Handle Identity
handleDoc = Attach.Handle {
    Attach.hLogger = logH,
    Attach.hReq = reqH,
    Attach.hParser = parserH,

    Attach.updateDoc = \doc -> Identity doc {
        document_id = 123,
        document_ownerId = 555,
        document_url = "https://server/link/222"
      }
}

spec_returnStickerId :: Spec
spec_returnStickerId = describe "Testing returnStickerId" $ do
    it "Should successfully (return Nothing) if its type is not 'sticker'" $ do
      attach <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) genNotStickerAttach)
      let result = Req.returnStickerId attach
      result `shouldBe` Nothing
    it "Should successfully (return StickerID) if its type is 'sticker'" $ do
      attachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) genNotStickerAttach)
      let stick = Sticker {sticker_id = 281}
          stickAttach = defaultAttach {attach_type = "sticker", attach_sticker = Just stick}
          allAttachs = case attachs of
            Nothing -> Just [stickAttach]
            Just _ -> (++) <$> Just [stickAttach] <*> attachs
          result = Req.returnStickerId allAttachs
          check = Just 281
      result `shouldBe` check
    it "Should successfully (return Nothing) to Nothing" $ do
      let result = Req.returnStickerId Nothing
      result `shouldBe` Nothing