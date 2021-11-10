module TestBot.Vk.Request where

import Control.Monad.Identity (Identity(..))
import Control.Monad.Catch (MonadThrow(..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Vk.GenData as GD
import qualified TestBot.Vk.Handlers as H

import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Vk.Parser.Objects.Attachment as Attach
import qualified Bot.Vk.Parser.Objects.Document as Doc
import qualified Bot.Vk.Parser.Objects.Photo as Photo
import qualified Bot.Vk.Parser.Objects.Video as Video
import qualified Bot.Vk.Parser.Objects.Audio as Audio
import qualified Bot.Vk.Parser.Objects.Geo as Geo
import qualified Bot.Vk.Parser.Objects.Sticker as Sticker

instance MonadThrow Identity where
  throwM e = undefined

spec_attachmentToString :: Spec
spec_attachmentToString =
  describe "Testing attachmentToString" $ do
    it "Should successfully convert Attachment with Doc to String" $ do
      let doc = Doc.Document {
            Doc.id = 781,
            Doc.owner_id = 129,
            Doc.title = "book.pdf",
            Doc.url = "https://server/link/222",
            Doc.access_key = "x\n 0\n \NAK^IMYz.<E"
          }
          docAttach = Attach.AttachDoc doc
          result = ReqSpec.attachmentToString docAttach
          check = "doc129_781"
      result `shouldBe` Just check
    it "Should successfully convert Attachment with Photo to String" $ do
      let photo = Photo.Photo {
            Photo.id = 810,
            Photo.owner_id = 589,
            Photo.access_key = "<97z\vYG\v$L"
          }
          photoAttach = Attach.AttachPhoto photo
          result = ReqSpec.attachmentToString photoAttach
          check = "photo589_810_<97z\vYG\v$L"
      result `shouldBe` Just check
    it "Should successfully convert Attachment with Video to String" $ do
      let video = Video.Video {
            Video.id = 567,
            Video.owner_id = 387,
            Video.access_key = "\DC47H)S:~:*3"
          }
          videoAttach = Attach.AttachVideo video
          result = ReqSpec.attachmentToString videoAttach
          check = "video387_567_\DC47H)S:~:*3"
      result `shouldBe` Just check
    it "Should successfully convert Attachment with Audio to String" $ do
      let audio = Audio.Audio {
            Audio.id = 787,
            Audio.owner_id = 861
          }
          audioAttach = Attach.AttachAudio audio
          result = ReqSpec.attachmentToString audioAttach
          check = "audio861_787"
      result `shouldBe` Just check
    it "Should successfully return '' for accidental Attachment \
       \if its type is not in ['doc','photo','video','audio']" $ do
      attach <- Gen.sample GD.genCompAttach
      let result = ReqSpec.attachmentToString attach
      result `shouldBe` Nothing

spec_attachmentsToQuery :: Spec
spec_attachmentsToQuery =
  describe "Testing attachmentsToQuery" $ do
    let doc = Doc.Document {
          Doc.id = 781, 
          Doc.owner_id = 129,
          Doc.title = "book.pdf",
          Doc.url = "https://server/link/222",
          Doc.access_key = "x\n 0\n \NAK^IMYz.<E"
        }
        docAttach = Attach.AttachDoc doc
        photo = Photo.Photo {
          Photo.id = 810,
          Photo.owner_id = 589,
          Photo.access_key = "<97z\vYG\v$L"
        }
        photoAttach = Attach.AttachPhoto photo
        audio = Audio.Audio {
          Audio.id = 787,
          Audio.owner_id = 861
        }
        audioAttach = Attach.AttachAudio audio
        video = Video.Video {
          Video.id = 567,
          Video.owner_id = 387,
          Video.access_key = "\DC47H)S:~:*3"
        }
        videoAttach = Attach.AttachVideo video
        attachs = Just [docAttach, photoAttach, videoAttach, audioAttach]
        check = Just "doc129_781,photo589_810_<97z\vYG\v$L,\
                     \video387_567_\DC47H)S:~:*3,audio861_787"
    it "Should successfully convert [Attachment] with type \
       \in ['doc','photo','video','audio'] to Query string" $ do
      let result = ReqSpec.attachmentsToQuery attachs
      result `shouldBe` check
    it "Should successfully convert [Attachment] with type \
       \in ['doc','photo','video','audio'] and accidental [Attachment] \
       \if its type is not in ['doc','photo','video','audio'] \
       \to Query string" $ do
      compAttachs <- Gen.sample $
        Gen.maybe (Gen.list (Range.constant 0 10) GD.genCompAttach)
      let allAttachs = case compAttachs of
            Nothing -> attachs
            Just _ -> (++) <$> attachs <*> compAttachs
          result = ReqSpec.attachmentsToQuery allAttachs
      result `shouldBe` check
    it "Should successfully convert accidental [Attachment] \
       \if its type is not in ['doc','photo','video','audio'] to Nothing" $ do
      compAttachs <- Gen.sample $
        Gen.maybe (Gen.list (Range.constant 0 10) GD.genCompAttach)
      let result = ReqSpec.attachmentsToQuery compAttachs
      result `shouldBe` Nothing
    it "Should successfully convert Nothing to Nothing" $ do
      let result = ReqSpec.attachmentsToQuery Nothing
      result `shouldBe` Nothing

spec_returnStickerId :: Spec
spec_returnStickerId =
  describe "Testing returnStickerId" $ do
    it "Should successfully (return Nothing) if its type is not 'sticker'" $ do
      attach <- Gen.sample $
        Gen.maybe (Gen.list (Range.constant 0 10) GD.genNotStickerAttach)
      let result = ReqSpec.returnStickerId attach
      result `shouldBe` Nothing
    it "Should successfully (return StickerID) if its type is 'sticker'" $ do
      attachs <- Gen.sample $
        Gen.maybe (Gen.list (Range.constant 0 10) GD.genNotStickerAttach)
      let sticker = Sticker.Sticker {
            Sticker.id = 281
          }
          stickAttach = Attach.AttachSticker sticker
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
spec_createServerQuery =
  describe "Testing createServerQuery" $ do
    it "Should successfully Server query string with group_id==Just a" $ do
      let reqH2 = H.reqH {ReqSpec.configReq = H.runCVk}
          result = ReqSpec.createServerQuery reqH2
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                 \group_id=37891&\
                                 \v=5.81"

spec_createHelpMessage :: Spec
spec_createHelpMessage =
  describe "Testing createHelpMessage" $ do
    it "Should successfully create Help Message" $ do
      let result = ReqSpec.createHelpMessage H.reqH 12345
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                  \message=Hi%21%20I%27m%20bot%3D%29&\
                                  \user_id=12345&\
                                  \v=5.81"

spec_createEchoMessage :: Spec
spec_createEchoMessage =
  describe "Testing createEchoMessage" $ do
    let doc = Doc.Document {
          Doc.id = 781,
          Doc.owner_id = 129,
          Doc.title = "book.pdf",
          Doc.url = "https://server/link/222",
          Doc.access_key = "x\n 0\n \NAK^IMYz.<E"
        }
        docAttach = Attach.AttachDoc doc
        photo = Photo.Photo {
          Photo.id = 810,
          Photo.owner_id = 589,
          Photo.access_key = "<97z\vYG\v$L"
        }
        photoAttach = Attach.AttachPhoto photo
        geo = Geo.Geo {
          Geo.coordinates = "99.999 109.107"
        }
    it "Should successfully create Echo Message without Geo, \
       \without [Attachment]" $ do
      let result = ReqSpec.createEchoMessage H.reqH 12345 "Hi!" Nothing Nothing
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                 \message=Hi%21&\
                                 \user_id=12345&\
                                 \v=5.81"
    it "Should successfully create Echo Message without Geo, \
       \with 1 Attachment" $ do
      let result = ReqSpec.createEchoMessage 
            H.reqH 12345 "Hi!" (Just [docAttach]) Nothing
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                  \attachment=doc129_781&\
                                  \message=Hi%21&\
                                  \user_id=12345&\
                                  \v=5.81"
    it "Should successfully create Echo Message without [Attachment], \
       \with Geo" $ do
      let result = ReqSpec.createEchoMessage
            H.reqH 12345 "Hi!" Nothing (Just geo)
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                  \lat=99.999&\
                                  \long=109.107&\
                                  \message=Hi%21&\
                                  \user_id=12345&\
                                  \v=5.81"
    it "Should successfully create Echo Message with Geo, \
       \with 1 Attachment" $ do
      let result = ReqSpec.createEchoMessage
            H.reqH 12345 "Hi!" (Just [docAttach]) (Just geo)
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                 \attachment=doc129_781&\
                                 \lat=99.999&\
                                 \long=109.107&\
                                 \message=Hi%21&\
                                 \user_id=12345&\
                                 \v=5.81"
    it "Should successfully create Echo Message with Geo, \
       \with [Attachment]" $ do
      let result = ReqSpec.createEchoMessage
            H.reqH 12345 "Hi!" (Just [docAttach, photoAttach]) (Just geo)
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                 \attachment=\
                                 \doc129_781%2Cphoto589_810_%3C97z%0BYG%0B%24L&\
                                 \lat=99.999&\
                                 \long=109.107&\
                                 \message=Hi%21&\
                                 \user_id=12345&\
                                 \v=5.81"

spec_geoToLatLong :: Spec
spec_geoToLatLong =
  describe "Testing geoToLatLong" $ do
    let geo = Geo.Geo {
      Geo.coordinates = "99.999 109.107"
    }
    it "Should successfully return Nothing if input is Nothing" $ do
      let result = ReqSpec.geoToLatLong Nothing
      result `shouldBe` [Nothing, Nothing]
    it "Should successfully return [Maybe Double]" $ do
      let result = ReqSpec.geoToLatLong (Just geo)
      result `shouldBe` [Just 99.999,Just 109.107]

spec_createRepeatMessage :: Spec
spec_createRepeatMessage =
  describe "Testing createRepeatMessage" $ do
    it "Should successfully create Repeat Message" $ do
      let result = ReqSpec.createRepeatMessage H.reqH 12345
      result `shouldBe` Identity "access_token=abcd0dcba&\
                                 \message=How%20many%20replies%20\
                                 \do%20you%20prefer%20to%20receive%3F&\
                                 \user_id=12345&\
                                 \v=5.81&\
                                 \keyboard="