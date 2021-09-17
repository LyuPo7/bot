{-# LANGUAGE OverloadedStrings #-}

module TestBot.Vk.Attach where

import Control.Monad.Identity

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hspec ()
import Test.Hspec

import TestBot.GenData as GD
import TestBot.Handlers as H

import qualified Bot.Vk.Request.AttachSpec as AttachSpec
import Bot.Vk.Parser.Data

spec_updateAttachment :: Spec
spec_updateAttachment = describe "Testing updateAttachment" $ do
    it "Should successfully (return Attachment) if its type is not 'doc'" $ do
      attach <- Gen.sample GD.genNotDocAttach
      let result = AttachSpec.updateAttachment H.attachH1 attach
      result `shouldBe` (Identity attach)
    it "Should successfully update Attachment if its type is 'doc'" $ do
      let doc = Document {document_id = 781, document_ownerId = 129, document_title = "book.pdf", document_url = "https://server/link/222", document_accessKey = "x\n 0\n \NAK^IMYz.<E"}
          docAttach = defaultAttach {attach_type = "doc", attach_doc = Just doc}
          result = AttachSpec.updateAttachment H.attachH2 docAttach
          newDoc = doc {document_id = 123, document_ownerId = 555, document_url = "https://server/link/222"}
          newDocAttach = docAttach {attach_doc = Just newDoc}
      result `shouldBe` (Identity newDocAttach)

spec_updateAttachments :: Spec
spec_updateAttachments = describe "Testing updateAttachments" $ do
    it "Should successfully (return [Attachment]) if its type is not 'doc'" $ do
      attachs <- Gen.sample $ Gen.maybe (Gen.list (Range.constant 0 10) GD.genNotDocAttach)
      let result = AttachSpec.updateAttachments H.attachH1 attachs
      result `shouldBe` (Identity attachs)
    it "Should successfully return Nothing to Nothing" $ do
      let result = AttachSpec.updateAttachments H.attachH1 Nothing
      result `shouldBe` (Identity Nothing)