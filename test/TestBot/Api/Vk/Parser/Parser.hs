module TestBot.Api.Vk.Parser.Parser where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Api.Vk.Handlers as VkHandlers

import qualified Bot.Api.Vk.Parser.Parser as VkParser
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing parseUpdateData for vk bot" $ do
    it "Should successfully parse UpdateData" $ do
      let result = VkParser.parseUpdateData VkHandlers.parserH bstr
          check = read (B8.toString obj) :: VkUpData.UpdateData
      result `shouldBe` Just check
    it "Should fail parse UpdateData" $ do
      let result = VkParser.parseUpdateData VkHandlers.parserH bstrFail
      result `shouldBe` (Just 
        $ VkUpData.UpdateData {
            VkUpData.ts = "0",
            VkUpData.updates = []
          }
        )

spec_parsePollResponse :: B.ByteString -> B.ByteString -> Spec
spec_parsePollResponse obj bstr = describe "Testing parsePollResponse for vk bot" $ do
    it "Should successfully parse PollResponse" $ do
      let result = VkParser.parsePollResponse VkHandlers.parserH bstr
          check = read (B8.toString obj) :: VkPollResp.PollResponse
      result `shouldBe` Just (Right check)

spec_parseUploadUrl :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadUrl obj bstr bstrFail = describe "Testing parseUploadUrl for vk bot" $ do
    it "Should successfully parse UploadUrlResponse" $ do
      let result = VkParser.parseUploadUrl VkHandlers.parserH bstr
          check = read (B8.toString obj) :: VkUpUrlResp.UploadUrlResponse
      result `shouldBe` Just check
    it "Should fail parse UploadUrlResponse" $ do
      let result = VkParser.parseUploadUrl VkHandlers.parserH bstrFail
      result `shouldBe` (Just 
        $ VkUpUrlResp.UploadUrlResponse {
            VkUpUrlResp.response = Nothing
          }
        )

spec_parseUploadFile :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadFile obj bstr bstrFail = describe "Testing parseUploadFile for vk bot" $ do
    it "Should successfully parse UploadFileResponse" $ do
      let result = VkParser.parseUploadFile VkHandlers.parserH bstr
          check = read (B8.toString obj) :: VkUpFileResp.UploadFileResponse
      result `shouldBe` Just check
    it "Should fail parse UploadFileResponse" $ do
      let result = VkParser.parseUploadFile VkHandlers.parserH bstrFail
      result `shouldBe` (Just 
        $ VkUpFileResp.UploadFileResponse {
            VkUpFileResp.file = Nothing
          }
        )

spec_parseUploadObject :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadObject obj bstr bstrFail = describe "Testing parseUploadObject for vk bot" $ do
    it "Should successfully parse UploadObjectResponse" $ do
      let result = VkParser.parseUploadObject VkHandlers.parserH bstr
          check = read (B8.toString obj) :: VkUpObjResp.UploadObjectResponse
      result `shouldBe` Just check
    it "Should fail parse UploadObjectResponse" $ do
      let result = VkParser.parseUploadObject VkHandlers.parserH bstrFail
      result `shouldBe` (Just 
        $ VkUpObjResp.UploadObjectResponse {
            VkUpObjResp.response = []
          }
        )