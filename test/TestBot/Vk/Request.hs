{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Request where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec ()
import Test.Hspec

import qualified TestBot.GenData as GD

import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import Bot.Vk.Parser.Data

spec_attachmentToString :: Spec
spec_attachmentToString = describe "Testing attachmentToString" $ do
    it "Should successfully convert Attachment with Doc to String" $ do
      let doc = Document {document_id = 781, document_ownerId = 129, document_title = "book.pdf", document_url = "https://server/link/222", document_accessKey = "x\n 0\n \NAK^IMYz.<E"}
          docAttach = defaultAttach {attach_type = "doc", attach_doc = Just doc}
          result = ReqSpec.attachmentToString docAttach
          check = "doc129_781"
      result `shouldBe` check
    it "Should successfully convert Attachment with Photo to String" $ do
      let photo = Photo {photo_id = 810, photo_ownerId = 589, photo_accessKey = "<97z\vYG\v$L"}
          photoAttach = defaultAttach {attach_type = "photo", attach_photo = Just photo}
          result = ReqSpec.attachmentToString photoAttach
          check = "photo589_810_<97z\vYG\v$L"
      result `shouldBe` check
    it "Should successfully convert Attachment with Video to String" $ do
      let video = Video {video_id = 567, video_ownerId = 387, video_accessKey = "\DC47H)S:~:*3"}
          videoAttach = defaultAttach {attach_type = "video", attach_video = Just video}
          result = ReqSpec.attachmentToString videoAttach
          check = "video387_567_\DC47H)S:~:*3"
      result `shouldBe` check
    it "Should successfully convert Attachment with Audio to String" $ do
      let audio = Audio {audio_id = 787, audio_ownerId = 861}
          audioAttach = defaultAttach {attach_type = "audio", attach_audio = Just audio}
          result = ReqSpec.attachmentToString audioAttach
          check = "audio861_787"
      result `shouldBe` check
    it "Should successfully return '' for accidental Attachment if its type is not in ['doc','photo','video','audio']" $ do
      attach <- Gen.sample GD.genCompAttach
      let result = ReqSpec.attachmentToString attach
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
      let result = ReqSpec.attachmentsToQuery attachs
      result `shouldBe` check
    it "Should successfully convert [Attachment] with type in ['doc','photo','video','audio'] and accidental [Attachment] if its type is not in ['doc','photo','video','audio'] to Query string" $ do
      compAttachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) GD.genCompAttach)
      let allAttachs = case compAttachs of
            Nothing -> attachs
            Just _ -> (++) <$> attachs <*> compAttachs
          result = ReqSpec.attachmentsToQuery allAttachs
      result `shouldBe` check
    it "Should successfully convert accidental [Attachment] if its type is not in ['doc','photo','video','audio'] to Nothing" $ do
      compAttachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) GD.genCompAttach)
      let result = ReqSpec.attachmentsToQuery compAttachs
      result `shouldBe` Nothing
    it "Should successfully convert Nothing to Nothing" $ do
      let result = ReqSpec.attachmentsToQuery Nothing
      result `shouldBe` Nothing

spec_returnStickerId :: Spec
spec_returnStickerId = describe "Testing returnStickerId" $ do
    it "Should successfully (return Nothing) if its type is not 'sticker'" $ do
      attach <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) GD.genNotStickerAttach)
      let result = ReqSpec.returnStickerId attach
      result `shouldBe` Nothing
    it "Should successfully (return StickerID) if its type is 'sticker'" $ do
      attachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) GD.genNotStickerAttach)
      let stick = Sticker {sticker_id = 281}
          stickAttach = defaultAttach {attach_type = "sticker", attach_sticker = Just stick}
          allAttachs = case attachs of
            Nothing -> Just [stickAttach]
            Just _ -> (++) <$> Just [stickAttach] <*> attachs
          result = ReqSpec.returnStickerId allAttachs
          check = Just 281
      result `shouldBe` check
    it "Should successfully (return Nothing) to Nothing" $ do
      let result = ReqSpec.returnStickerId Nothing
      result `shouldBe` Nothing