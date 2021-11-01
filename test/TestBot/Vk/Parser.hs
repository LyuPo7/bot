module TestBot.Vk.Parser where

import Control.Monad.Identity (Identity(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Vk.Handlers as H

import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Parser.Data as PD

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing parseUpdateData for vk bot" $ do
    it "Should successfully parse UpdateData" $ do
      let result = Parser.parseUpdateData H.parserH bstr
          check = read (B8.toString obj) :: PD.UpdateData
      result `shouldBe` Identity check
    it "Should fail parse UpdateData" $ do
      let result = Parser.parseUpdateData H.parserH bstrFail
      result `shouldBe` (Identity 
        $ PD.UpdateData {
            PD.ts = "0", PD.updates = []
          }
        )

spec_parsePollResponse :: B.ByteString -> B.ByteString -> Spec
spec_parsePollResponse obj bstr = describe "Testing parsePollResponse for vk bot" $ do
    it "Should successfully parse PollResponse" $ do
      let result = Parser.parsePollResponse H.parserH bstr
          check = read (B8.toString obj) :: PD.PollResponse
      result `shouldBe` Identity (Right check)

spec_parseUploadUrl :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadUrl obj bstr bstrFail = describe "Testing parseUploadUrl for vk bot" $ do
    it "Should successfully parse UploadUrlResponse" $ do
      let result = Parser.parseUploadUrl H.parserH bstr
          check = read (B8.toString obj) :: PD.UploadUrlResponse
      result `shouldBe` Identity check
    it "Should fail parse UploadUrlResponse" $ do
      let result = Parser.parseUploadUrl H.parserH bstrFail
      result `shouldBe` (Identity 
        $ PD.UploadUrlResponse {
            PD.upUrlResponse_response = Nothing
          }
        )

spec_parseUploadFile :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadFile obj bstr bstrFail = describe "Testing parseUploadFile for vk bot" $ do
    it "Should successfully parse UploadFileResponse" $ do
      let result = Parser.parseUploadFile H.parserH bstr
          check = read (B8.toString obj) :: PD.UploadFileResponse
      result `shouldBe` Identity check
    it "Should fail parse UploadFileResponse" $ do
      let result = Parser.parseUploadFile H.parserH bstrFail
      result `shouldBe` (Identity 
        $ PD.UploadFileResponse {
            PD.upFileResponse_file = Nothing
          }
        )

spec_parseUploadObject :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadObject obj bstr bstrFail = describe "Testing parseUploadObject for vk bot" $ do
    it "Should successfully parse UploadObjectResponse" $ do
      let result = Parser.parseUploadObject H.parserH bstr
          check = read (B8.toString obj) :: PD.UploadObjectResponse
      result `shouldBe` Identity check
    it "Should fail parse UploadObjectResponse" $ do
      let result = Parser.parseUploadObject H.parserH bstrFail
      result `shouldBe` (Identity 
        $ PD.UploadObjectResponse {
            PD.upObjResponse_response = []
          }
        )