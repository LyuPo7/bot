{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Proba2 where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as B8
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T

import Hedgehog (Gen, Property, property, (===), forAll)
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import Test.Tasty.Hspec ()
import Test.Hspec
import Control.Monad.Identity()

import qualified Bot.Logger as Logger
import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
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

-- | Config file
updatesFile, update_objFile :: FilePath
updatesFile = "src/Bot/files/Test/Vk/Parser/UpdateData.json"
update_objFile = "src/Bot/files/Test/Vk/Parser/UpdateDataObj.txt"

-- | Read the config JSON file.
readUpdate, readobjFile :: IO B.ByteString
readUpdate = B.readFile updatesFile
readobjFile = B.readFile update_objFile

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing update parse for vk bot" $ do
    it "Should successfully parse UpdateData" $ do
      let result = Parser.parseUpdateData parserH bstr
          check = read (B8.toString obj) :: UpdateData
      result `shouldBe` (Identity check)
    it "Should fail parse UpdateData" $ do
      let result = Parser.parseUpdateData parserH bstrFail
      result `shouldBe` (Identity UpdateData{ts = "0", updates = [])