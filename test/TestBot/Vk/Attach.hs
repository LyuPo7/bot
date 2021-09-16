{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Attach where

import Control.Monad.Identity

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec ()
import Test.Hspec

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Request.AttachSpec as Attach
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

genNotDocAttach :: Gen Attachment
genNotDocAttach = Attachment
  <$> Gen.element ["photo", "audio", "video", "link", "market", "market_album", "wall", "wall_reply", "sticker", "gift"]
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

spec_attachmentToString :: Spec
spec_attachmentToString = describe "Testing attachmentToString" $ do
    it "Should successfully convert Attachment with Doc to String" $ do
      let doc = Document {document_id = 781, document_ownerId = 129, document_title = "book.pdf", document_url = "https://server/link/222", document_accessKey = "x\n 0\n \NAK^IMYz.<E"}
          docAttach = defaultAttach {attach_type = "doc", attach_doc = Just doc}
          result = Attach.attachmentToString docAttach
          check = "doc129_781"
      result `shouldBe` check
    it "Should successfully convert Attachment with Photo to String" $ do
      let photo = Photo {photo_id = 810, photo_ownerId = 589, photo_accessKey = "<97z\vYG\v$L"}
          photoAttach = defaultAttach {attach_type = "photo", attach_photo = Just photo}
          result = Attach.attachmentToString photoAttach
          check = "photo589_810_<97z\vYG\v$L"
      result `shouldBe` check
    it "Should successfully convert Attachment with Video to String" $ do
      let video = Video {video_id = 567, video_ownerId = 387, video_accessKey = "\DC47H)S:~:*3"}
          videoAttach = defaultAttach {attach_type = "video", attach_video = Just video}
          result = Attach.attachmentToString videoAttach
          check = "video387_567_\DC47H)S:~:*3"
      result `shouldBe` check
    it "Should successfully convert Attachment with Audio to String" $ do
      let audio = Audio {audio_id = 787, audio_ownerId = 861}
          audioAttach = defaultAttach {attach_type = "audio", attach_audio = Just audio}
          result = Attach.attachmentToString audioAttach
          check = "audio861_787"
      result `shouldBe` check
    it "Should successfully return '' for accidental Attachment if its type is not in ['doc','photo','video','audio']" $ do
      attach <- Gen.sample genCompAttach
      let result = Attach.attachmentToString attach
      result `shouldBe` ""

spec_attachmentsToQuery :: Spec
spec_attachmentsToQuery = describe "Testing attachmentsToQuery" $ do
    let doc = Document {document_id = 781, document_ownerId = 129, document_title = "book.pdf", document_url = "https://server/link/222", document_accessKey = "x\n 0\n \NAK^IMYz.<E"}
        docAttach = defaultAttach {attach_type = "doc", attach_doc = Just doc}
        photo = Photo {photo_id = 810, photo_ownerId = 589, photo_accessKey = "<97z\vYG\v$L"}
        photoAttach = defaultAttach {attach_type = "photo", attach_photo = Just photo}
        audio = Audio {audio_id = 787, audio_ownerId = 861}
        audioAttach = defaultAttach {attach_type = "audio", attach_audio = Just audio}
        video = Video {video_id = 567, video_ownerId = 387, video_accessKey = "\DC47H)S:~:*3"}
        videoAttach = defaultAttach {attach_type = "video", attach_video = Just video}
        attachs = Just [docAttach, photoAttach, videoAttach, audioAttach]
        check = Just "doc129_781,photo589_810_<97z\vYG\v$L,video387_567_\DC47H)S:~:*3,audio861_787"
    it "Should successfully convert [Attachment] with type in ['doc','photo','video','audio'] to Query string" $ do
      let result = Attach.attachmentsToQuery attachs
      result `shouldBe` check
    it "Should successfully convert [Attachment] with type in ['doc','photo','video','audio'] and accidental [Attachment] if its type is not in ['doc','photo','video','audio'] to Query string" $ do
      compAttachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) genCompAttach)
      let allAttachs = case compAttachs of
            Nothing -> attachs
            Just _ -> (++) <$> attachs <*> compAttachs
          result = Attach.attachmentsToQuery allAttachs
      result `shouldBe` check
    it "Should successfully convert accidental [Attachment] if its type is not in ['doc','photo','video','audio'] to Nothing" $ do
      compAttachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) genCompAttach)
      let result = Attach.attachmentsToQuery compAttachs
      result `shouldBe` Nothing
    it "Should successfully convert Nothing to Nothing" $ do
      let result = Attach.attachmentsToQuery Nothing
      result `shouldBe` Nothing

spec_updateAttachment :: Spec
spec_updateAttachment = describe "Testing updateAttachment" $ do
    it "Should successfully (return Attachment) if its type is not 'doc'" $ do
      attach <- Gen.sample genNotDocAttach
      let result = Attach.updateAttachment handle attach
      result `shouldBe` (Identity attach)
    it "Should successfully update Attachment if its type is 'doc'" $ do
      let doc = Document {document_id = 781, document_ownerId = 129, document_title = "book.pdf", document_url = "https://server/link/222", document_accessKey = "x\n 0\n \NAK^IMYz.<E"}
          docAttach = defaultAttach {attach_type = "doc", attach_doc = Just doc}
          result = Attach.updateAttachment handleDoc docAttach
          newDoc = doc {document_id = 123, document_ownerId = 555, document_url = "https://server/link/222"}
          newDocAttach = docAttach {attach_doc = Just newDoc}
      result `shouldBe` (Identity newDocAttach)

spec_updateAttachments :: Spec
spec_updateAttachments = describe "Testing updateAttachments" $ do
    it "Should successfully (return [Attachment]) if its type is not 'doc'" $ do
      attachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) genNotDocAttach)
      let result = Attach.updateAttachments handle attachs
      result `shouldBe` (Identity attachs)
    it "Should successfully return Nothing to Nothing" $ do
      let result = Attach.updateAttachments handle Nothing
      result `shouldBe` (Identity Nothing)