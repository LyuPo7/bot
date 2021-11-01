module TestBot.Vk.Attach where

import Control.Monad.Identity (Identity(..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, shouldBe, it, describe)

import TestBot.Vk.GenData as GD
import TestBot.Vk.Handlers as H

import qualified Bot.Vk.Request.AttachSpec as AttachSpec
import qualified Bot.Vk.Parser.Data as PD

spec_updateAttachment :: Spec
spec_updateAttachment = 
  describe "Testing updateAttachment" $ do
    it "Should successfully (return Attachment) if its type is not 'doc'" $ do
      attach <- Gen.sample GD.genNotDocAttach
      let result = AttachSpec.updateAttachment H.attachH1 attach
      result `shouldBe` Identity attach
    it "Should successfully update Attachment if its type is 'doc'" $ do
      let doc = PD.Document {
            PD.document_id = 781,
            PD.document_ownerId = 129,
            PD.document_title = "book.pdf",
            PD.document_url = "https://server/link/222",
            PD.document_accessKey = "x\n 0\n \NAK^IMYz.<E"
          }
          docAttach = PD.defaultAttach {
            PD.attach_type = "doc",
            PD.attach_doc = Just doc
          }
          result = AttachSpec.updateAttachment H.attachH2 docAttach
          newDoc = doc {
            PD.document_id = 123,
            PD.document_ownerId = 555,
            PD.document_url = "https://server/link/222"
          }
          newDocAttach = docAttach {
            PD.attach_doc = Just newDoc
          }
      result `shouldBe` Identity newDocAttach

spec_updateAttachments :: Spec
spec_updateAttachments = 
  describe "Testing updateAttachments" $ do
    it "Should successfully (return [Attachment]) if its type is not 'doc'" $ do
      attachs <- Gen.sample $
        Gen.maybe (Gen.list (Range.constant 0 10) GD.genNotDocAttach)
      let result = AttachSpec.updateAttachments H.attachH1 attachs
      result `shouldBe` Identity attachs
    it "Should successfully return Nothing to Nothing" $ do
      let result = AttachSpec.updateAttachments H.attachH1 Nothing
      result `shouldBe` Identity Nothing