{-# LANGUAGE OverloadedStrings #-}

module TestBot.Tele.Parser where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import TestBot.Tele.Handlers as H

import qualified Bot.Tele.Parser.Parser as Parser
import qualified Bot.Tele.Parser.Data as DParser

import Test.Tasty.Hspec ()
import Test.Hspec

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing update parse for telegram bot" $ do
    it "Should successfully parse updates" $ do
      let result = Parser.parseUpdateData H.parserH bstr
          check = read (B8.toString obj) :: DParser.UpdateData
      result `shouldBe` (Identity check)
    it "Should fail parse updates" $ do
      let result = Parser.parseUpdateData H.parserH bstrFail
      result `shouldBe` (Identity DParser.UpdateData {DParser.ok = True, DParser.result = []})