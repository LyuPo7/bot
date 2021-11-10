module TestBot.Vk.Parser where

import Control.Monad.Identity (Identity(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Vk.Handlers as H

import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Parser.Objects.UploadObjectResponse as UpObjResp
import qualified Bot.Vk.Parser.Objects.UploadFileResponse as UpFileResp
import qualified Bot.Vk.Parser.Objects.UploadUrlResponse as UpUrlResp
import qualified Bot.Vk.Parser.Objects.PollResponse as PollResp
import qualified Bot.Vk.Parser.Objects.UpdateData as UpData

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing parseUpdateData for vk bot" $ do
    it "Should successfully parse UpdateData" $ do
      let result = Parser.parseUpdateData H.parserH bstr
          check = read (B8.toString obj) :: UpData.UpdateData
      result `shouldBe` Identity check
    it "Should fail parse UpdateData" $ do
      let result = Parser.parseUpdateData H.parserH bstrFail
      result `shouldBe` (Identity 
        $ UpData.UpdateData {
            UpData.ts = "0",
            UpData.updates = []
          }
        )

spec_parsePollResponse :: B.ByteString -> B.ByteString -> Spec
spec_parsePollResponse obj bstr = describe "Testing parsePollResponse for vk bot" $ do
    it "Should successfully parse PollResponse" $ do
      let result = Parser.parsePollResponse H.parserH bstr
          check = read (B8.toString obj) :: PollResp.PollResponse
      result `shouldBe` Identity (Right check)

spec_parseUploadUrl :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadUrl obj bstr bstrFail = describe "Testing parseUploadUrl for vk bot" $ do
    it "Should successfully parse UploadUrlResponse" $ do
      let result = Parser.parseUploadUrl H.parserH bstr
          check = read (B8.toString obj) :: UpUrlResp.UploadUrlResponse
      result `shouldBe` Identity check
    it "Should fail parse UploadUrlResponse" $ do
      let result = Parser.parseUploadUrl H.parserH bstrFail
      result `shouldBe` (Identity 
        $ UpUrlResp.UploadUrlResponse {
            UpUrlResp.response = Nothing
          }
        )

spec_parseUploadFile :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadFile obj bstr bstrFail = describe "Testing parseUploadFile for vk bot" $ do
    it "Should successfully parse UploadFileResponse" $ do
      let result = Parser.parseUploadFile H.parserH bstr
          check = read (B8.toString obj) :: UpFileResp.UploadFileResponse
      result `shouldBe` Identity check
    it "Should fail parse UploadFileResponse" $ do
      let result = Parser.parseUploadFile H.parserH bstrFail
      result `shouldBe` (Identity 
        $ UpFileResp.UploadFileResponse {
            UpFileResp.file = Nothing
          }
        )

spec_parseUploadObject :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadObject obj bstr bstrFail = describe "Testing parseUploadObject for vk bot" $ do
    it "Should successfully parse UploadObjectResponse" $ do
      let result = Parser.parseUploadObject H.parserH bstr
          check = read (B8.toString obj) :: UpObjResp.UploadObjectResponse
      result `shouldBe` Identity check
    it "Should fail parse UploadObjectResponse" $ do
      let result = Parser.parseUploadObject H.parserH bstrFail
      result `shouldBe` (Identity 
        $ UpObjResp.UploadObjectResponse {
            UpObjResp.response = []
          }
        )