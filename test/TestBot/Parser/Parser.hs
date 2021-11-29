module TestBot.Parser.Parser where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8
import Data.Text (Text)

import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Api.Vk.Handlers as VkHandlers
import qualified TestBot.Api.Tele.Handlers as TeleHandlers

import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData

spec_parseData_TeleUpData_UpdateData :: B.ByteString ->
                                        B.ByteString ->
                                        B.ByteString ->
                                        Spec
spec_parseData_TeleUpData_UpdateData obj bstr bstrFail = 
  describe "Testing update parse for telegram bot" $ do
    it "Should successfully parseData with TeleUpData.UpdateData" $ do
      let result = BotParser.parseData TeleHandlers.dbqH bstr
          check = read (B8.toString obj) :: TeleUpData.UpdateData
      result `shouldBe` Just (Right check)
    it "Should fail parse updates" $ do
      let result = BotParser.parseData TeleHandlers.dbqH bstrFail
            :: Maybe (Either Text TeleUpData.UpdateData)
          err = "Error in $.result[1].message: \
                \parsing Bot.Api.Tele.Objects.Message.Message(Message) \
                \failed, key \"message_id\" not found"
      result `shouldBe` Just (Left err)

spec_parseData_VkUpData_UpdateData :: B.ByteString ->
                                      B.ByteString ->
                                      B.ByteString ->
                                      Spec
spec_parseData_VkUpData_UpdateData obj bstr bstrFail =
  describe "Testing parseData with VkUpData.UpdateData" $ do
    it "Should successfully parse VkUpData.UpdateData" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstr
          check = read (B8.toString obj) :: VkUpData.UpdateData
      result `shouldBe` Just (Right check)
    it "Should fail parse VkUpData.UpdateData" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstrFail
            :: Maybe (Either Text VkUpData.UpdateData)
          err = "Error in $.updates[2].object['user_id']: \
                \parsing Integer failed, expected Number, \
                \but encountered String"
      result `shouldBe` Just (Left err)

spec_parseData_VkPollResp_PollResponse :: B.ByteString ->
                                          B.ByteString ->
                                          Spec
spec_parseData_VkPollResp_PollResponse obj bstr =
    describe "Testing parseData with VkPollResp.PollResponse" $ do
    it "Should successfully parse VkPollResp.PollResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstr
            :: Maybe (Either Text VkPollResp.PollResponse)
          check = read (B8.toString obj)
      result `shouldBe` Just (Right check)

spec_parseData_VkUpUrlResp_UploadUrlResponse :: B.ByteString ->
                                                B.ByteString ->
                                                B.ByteString ->
                                                Spec
spec_parseData_VkUpUrlResp_UploadUrlResponse obj bstr bstrFail =
  describe "Testing parseData with VkUpUrlResp.UploadUrlResponse" $ do
    it "Should successfully parse VkUpUrlResp.UploadUrlResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstr
          check = read (B8.toString obj) :: VkUpUrlResp.UploadUrlResponse
      result `shouldBe` Just (Right check)
    it "Should fail parse VkUpUrlResp.UploadUrlResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstrFail
            :: Maybe (Either Text VkUpUrlResp.UploadUrlResponse)
          err = "Error in $.response['upload_url']: \
                \parsing Text failed, expected String, \
                \but encountered Number"
      result `shouldBe` Just (Left err)

spec_parseData_VkUpFileResp_UploadFileResponse :: B.ByteString ->
                                                  B.ByteString ->
                                                  B.ByteString ->
                                                  Spec
spec_parseData_VkUpFileResp_UploadFileResponse obj bstr bstrFail =
  describe "Testing parseData with VkUpFileResp.UploadFileResponse" $ do
    it "Should successfully parse VkUpFileResp.UploadFileResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstr
          check = read (B8.toString obj) :: VkUpFileResp.UploadFileResponse
      result `shouldBe` Just (Right check)
    it "Should fail parse VkUpFileResp.UploadFileResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstrFail
            :: Maybe (Either Text VkUpFileResp.UploadFileResponse)
          err = "Error in $.file: parsing Text failed, \
                \expected String, but encountered Number"
      result `shouldBe` Just (Left err)

spec_parseData_VkUpObjResp_UploadObjectResponse :: B.ByteString ->
                                                   B.ByteString ->
                                                   B.ByteString ->
                                                   Spec
spec_parseData_VkUpObjResp_UploadObjectResponse obj bstr bstrFail =
  describe "Testing parseData with VkUpObjResp.UploadObjectResponse" $ do
    it "Should successfully parse VkUpObjResp.UploadObjectResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstr
          check = read (B8.toString obj) :: VkUpObjResp.UploadObjectResponse
      result `shouldBe` Just (Right check)
    it "Should fail parse VkUpObjResp.UploadObjectResponse" $ do
      let result = BotParser.parseData VkHandlers.dbqH bstrFail
            :: Maybe (Either Text VkUpObjResp.UploadObjectResponse)
          err = "Error in $: parsing \
                \Bot.Api.Vk.Objects.UploadObjectResponse.\
                \UploadObjectResponse(UploadObjectResponse) \
                \failed, key \"response\" not found"
      result `shouldBe` Just (Left err)