module TestBot.Vk.Attach where

import Control.Monad.Identity (Identity(..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Vk.GenData as GD
import qualified TestBot.Vk.Handlers as H

import qualified Bot.Vk.Request.AttachSpec as AttachSpec
import qualified Bot.Vk.Parser.Objects.Attachment as Attach
import qualified Bot.Vk.Parser.Objects.Document as Doc

spec_updateAttachment :: Spec
spec_updateAttachment = 
  describe "Testing updateAttachment" $ do
    it "Should successfully (return Attachment) if its type is not 'doc'" $ do
      attach <- Gen.sample GD.genNotDocAttach
      let result = AttachSpec.updateAttachment H.attachH1 attach
      result `shouldBe` Identity attach
    it "Should successfully update Attachment if its type is 'doc'" $ do
      let doc = Doc.Document {
            Doc.id = 781,
            Doc.owner_id = 129,
            Doc.title = "book.pdf",
            Doc.url = "https://server/link/222",
            Doc.access_key = "x\n 0\n \NAK^IMYz.<E"
          }
          docAttach = Attach.AttachDoc doc
          result = AttachSpec.updateAttachment H.attachH2 docAttach
          newDoc = doc {
            Doc.id = 123,
            Doc.owner_id = 555,
            Doc.url = "https://server/link/222"
          }
          newDocAttach = Attach.AttachDoc newDoc
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