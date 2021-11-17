module TestBot.Api.Tele.Parser.Parser where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import qualified TestBot.Api.Tele.Handlers as TeleHandlers

import qualified Bot.Api.Tele.Parser.Parser as Parser
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData

import Test.Hspec (Spec, shouldBe, it, describe)

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = 
  describe "Testing update parse for telegram bot" $ do
    it "Should successfully parse updates" $ do
      let result = Parser.parseUpdateData TeleHandlers.parserH bstr
          check = read (B8.toString obj) :: TeleUpData.UpdateData
      result `shouldBe` Just check
    it "Should fail parse updates" $ do
      let result = Parser.parseUpdateData TeleHandlers.parserH bstrFail
      result `shouldBe` Just TeleUpData.UpdateData {
          TeleUpData.ok = True,
          TeleUpData.result = []
        }