{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Request where

import Control.Monad.Identity

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec ()
import Test.Hspec

import qualified TestBot.GenData as GD
import qualified TestBot.Handlers as H

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
  
spec_createServerQuery :: Spec
spec_createServerQuery = describe "Testing createServerQuery" $ do
    it "Should successfully Server query string using 0 if group_id==Nothing" $ do
      let result = ReqSpec.createServerQuery H.reqH
      result `shouldBe` (Identity "access_token=abcd0dcba&group_id=0&v=5.81")
    it "Should successfully Server query string with group_id==Just a" $ do
      let reqH2 = H.reqH {ReqSpec.configReq = H.runCVk}
          result = ReqSpec.createServerQuery reqH2
      result `shouldBe` (Identity "access_token=abcd0dcba&group_id=37891&v=5.81")

spec_createHelpMessage :: Spec
spec_createHelpMessage = describe "Testing createHelpMessage" $ do
    it "Should successfully create Help Message" $ do
      let result = ReqSpec.createHelpMessage H.reqH 12345
      result `shouldBe` (Identity "access_token=abcd0dcba&message=Hi%21%20I%27m%20bot%3D%29&user_id=12345&v=5.81")

spec_createEchoMessage :: Spec
spec_createEchoMessage = describe "Testing createEchoMessage" $ do
    let doc = Document {document_id = 781, document_ownerId = 129, document_title = "book.pdf", document_url = "https://server/link/222", document_accessKey = "x\n 0\n \NAK^IMYz.<E"}
        docAttach = defaultAttach {attach_type = "doc", attach_doc = Just doc}
        photo = Photo {photo_id = 810, photo_ownerId = 589, photo_accessKey = "<97z\vYG\v$L"}
        photoAttach = defaultAttach {attach_type = "photo", attach_photo = Just photo}
        geo = Geo {geo_coordinates = "99.999 109.107"}
    it "Should successfully create Echo Message without Geo, without [Attachment]" $ do
      let result = ReqSpec.createEchoMessage H.reqH 12345 "Hi!" Nothing Nothing
      result `shouldBe` (Identity "access_token=abcd0dcba&message=Hi%21&user_id=12345&v=5.81")
    it "Should successfully create Echo Message without Geo, with 1 Attachment" $ do
      let result = ReqSpec.createEchoMessage H.reqH 12345 "Hi!" (Just [docAttach]) Nothing
      result `shouldBe` (Identity "access_token=abcd0dcba&attachment=doc129_781&message=Hi%21&user_id=12345&v=5.81")
    it "Should successfully create Echo Message without [Attachment], with Geo" $ do
      let result = ReqSpec.createEchoMessage H.reqH 12345 "Hi!" Nothing (Just geo)
      result `shouldBe` (Identity "access_token=abcd0dcba&lat=99.999&long=109.107&message=Hi%21&user_id=12345&v=5.81")
    it "Should successfully create Echo Message with Geo, with 1 Attachment" $ do
      let result = ReqSpec.createEchoMessage H.reqH 12345 "Hi!" (Just [docAttach]) (Just geo)
      result `shouldBe` (Identity "access_token=abcd0dcba&attachment=doc129_781&lat=99.999&long=109.107&message=Hi%21&user_id=12345&v=5.81")
    it "Should successfully create Echo Message with Geo, with [Attachment]" $ do
      let result = ReqSpec.createEchoMessage H.reqH 12345 "Hi!" (Just [docAttach, photoAttach]) (Just geo)
      result `shouldBe` (Identity "access_token=abcd0dcba&attachment=doc129_781%2Cphoto589_810_%3C97z%0BYG%0B%24L&lat=99.999&long=109.107&message=Hi%21&user_id=12345&v=5.81")

spec_geoToLatLong :: Spec
spec_geoToLatLong = describe "Testing geoToLatLong" $ do
    let geo = Geo {geo_coordinates = "99.999 109.107"}
    it "Should successfully return Nothing if input is Nothing" $ do
      let result = ReqSpec.geoToLatLong Nothing
      result `shouldBe` [Nothing, Nothing]
    it "Should successfully return [Maybe Double]" $ do
      let result = ReqSpec.geoToLatLong (Just geo)
      result `shouldBe` [Just 99.999,Just 109.107]

spec_createRepeatMessage :: Spec
spec_createRepeatMessage = describe "Testing createRepeatMessage" $ do
    it "Should successfully create Repeat Message" $ do
      let result = ReqSpec.createRepeatMessage H.reqH 12345
      result `shouldBe` (Identity "access_token=abcd0dcba&message=How%20many%20replies%20do%20you%20prefer%20to%20recieve%3F&user_id=12345&v=5.81&keyboard=")