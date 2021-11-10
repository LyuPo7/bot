module TestBot.Vk.Document where

import Control.Monad.Identity (Identity(..))

import qualified Hedgehog.Gen as Gen
import Test.Hspec (Spec, shouldBe, it, describe)

import qualified TestBot.Vk.GenData as GD
import qualified TestBot.Vk.Handlers as H

import qualified Bot.Vk.Request.DocumentSpec as DocSpec
import qualified Bot.Vk.Parser.Objects.Document as Doc

spec_updateDoc :: Spec
spec_updateDoc=
  describe "Testing updateDoc" $ do
    it "Should successfully change Document if \
       \saveUploadedDoc return 1 correct Object" $ do
      doc <- Gen.sample GD.genDoc
      let result = DocSpec.updateDoc H.docH1 doc
      result `shouldBe` Identity doc {
          Doc.url = "https://lp.vk.com/link/12gh56",
          Doc.id = 123,
          Doc.owner_id = 12321
        }
    it "Should successfully return Document without changes \
      \if saveUploadedDoc return more than 1 correct Object" $ do
      doc <- Gen.sample GD.genDoc
      let docH2 = H.docH1 {
            DocSpec.saveUploadedDoc = \_ ->
              Identity "{\"response\" : [\n{\n\"id\":123,\n\
                       \\"owner_id\":12321,\n\
                       \\"url\":\"https:\\/\\/lp.vk.com\\/link\\/12gh56\"\n},\
                       \\\n{\n\"id\":555,\n\
                       \\"owner_id\":909089,\n\
                       \\"url\":\"https:\\/\\/lp.vk.com\\/link\\/ad1f2ghjk\"\n}\n]}"
                      }
          result = DocSpec.updateDoc docH2 doc
      result `shouldBe` Identity doc
    it "Should successfully return Document without changes \
       \if saveUploadedDoc return incorrect Object" $ do
      doc <- Gen.sample GD.genDoc
      let docH3 = H.docH1 {
            DocSpec.saveUploadedDoc = \_ -> Identity "{\"response\" : []}"
          }
          result = DocSpec.updateDoc docH3 doc
      result `shouldBe` Identity doc
    