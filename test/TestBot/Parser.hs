{-# LANGUAGE OverloadedStrings #-}

module TestBot.Parser where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B8

import qualified Bot.Logger as Logger
import qualified Bot.Tele.Parser.Parser as Parser
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import qualified Bot.Tele.Parser.Data as DParser

import Test.Tasty.Hspec ()
import Test.Hspec

spec_parseUpdateData :: B.ByteString -> B.ByteString -> B.ByteString -> Spec
spec_parseUpdateData obj bstr bstrFail = describe "Testing update parse for telegram bot" $ do
    it "Should successfully parse updates" $ do
      let result = Parser.parseUpdateData parserH bstr
          check = read (B8.toString obj) :: DParser.UpdateData
      result `shouldBe` (Identity check)
    it "Should fail parse updates" $ do
      let result = Parser.parseUpdateData parserH bstrFail
      result `shouldBe` (Identity DParser.UpdateData {DParser.ok = True, DParser.result = []})

logH :: Logger.Handle Identity
logH = Logger.Handle {
    Logger.log = \_ _ -> return (),
    Logger.hconfig = Logger.Config {Logger.cVerbocity = Nothing}
}

parserH :: ParserSpec.Handle Identity
parserH = ParserSpec.Handle {
    ParserSpec.hLogger = logH
}