{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Parser where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import TestBot.Vk.Handlers

import qualified Bot.Vk.Parser.Parser as Parser
import qualified Bot.Vk.Parser.Data as DParser

import Test.Tasty.Hspec ()
import Test.Hspec

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing parseUpdateData for vk bot" $ do
    it "Should successfully parse UpdateData" $ do
      let result = Parser.parseUpdateData parserH bstr
          check = read (B8.toString obj) :: DParser.UpdateData
      result `shouldBe` (Identity check)
    it "Should fail parse UpdateData" $ do
      let result = Parser.parseUpdateData parserH bstrFail
      result `shouldBe` (Identity DParser.UpdateData {DParser.ts = "0", DParser.updates = []})

spec_parsePollResponse :: B.ByteString -> B.ByteString -> Spec
spec_parsePollResponse obj bstr = describe "Testing parsePollResponse for vk bot" $ do
    it "Should successfully parse PollResponse" $ do
      let result = Parser.parsePollResponse parserH bstr
          check = read (B8.toString obj) :: DParser.PollResponse
      result `shouldBe` (Identity $ Right check)

spec_parseUploadUrl :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadUrl obj bstr bstrFail = describe "Testing parseUploadUrl for vk bot" $ do
    it "Should successfully parse UploadUrlResponse" $ do
      let result = Parser.parseUploadUrl parserH bstr
          check = read (B8.toString obj) :: DParser.UploadUrlResponse
      result `shouldBe` (Identity check)
    it "Should fail parse UploadUrlResponse" $ do
      let result = Parser.parseUploadUrl parserH bstrFail
      result `shouldBe` (Identity $ DParser.UploadUrlResponse {DParser.upUrlResponse_response = Nothing})

spec_parseUploadFile :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadFile obj bstr bstrFail = describe "Testing parseUploadFile for vk bot" $ do
    it "Should successfully parse UploadFileResponse" $ do
      let result = Parser.parseUploadFile parserH bstr
          check = read (B8.toString obj) :: DParser.UploadFileResponse
      result `shouldBe` (Identity check)
    it "Should fail parse UploadFileResponse" $ do
      let result = Parser.parseUploadFile parserH bstrFail
      result `shouldBe` (Identity $ DParser.UploadFileResponse {DParser.upFileResponse_file = Nothing})

spec_parseUploadObject :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUploadObject obj bstr bstrFail = describe "Testing parseUploadObject for vk bot" $ do
    it "Should successfully parse UploadObjectResponse" $ do
      let result = Parser.parseUploadObject parserH bstr
          check = read (B8.toString obj) :: DParser.UploadObjectResponse
      result `shouldBe` (Identity check)
    it "Should fail parse UploadObjectResponse" $ do
      let result = Parser.parseUploadObject parserH bstrFail
      result `shouldBe` (Identity $ DParser.UploadObjectResponse {DParser.upObjResponse_response = []})