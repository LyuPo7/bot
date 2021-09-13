import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Hedgehog
import qualified Data.ByteString.Lazy as B

import TestBot.Parser
import TestBot.Config
import TestBot.Run
--import Props
--import GoldenTests

props :: [TestTree]
props = [
  testProperty "Successfull checkConfig for each valid Config" prop_checkConfig,
  testProperty "Unsuccessfull answerMode parse for each text Message while waiting answer number" prop_answerModeFail,
  testProperty "Successfull answerMode parse for each Message with number 5 in body while waiting answer number" prop_answerModeSuc,
  testProperty "Successfull replyMode for /help, /start bot command" prop_replyModeReplySuc,
  testProperty "Successfull replyMode for /reply bot command" prop_replyModeAnswerSuc,
  testProperty "Successfull replyMode for message without bot command" prop_replyModeOrdinarySuc
  ]

main :: IO ()
--main = defaultMain $ testGroup "(no tests)" []
main = do
  bstr <- readUpdate
  bstrFail <- readUpdateFail
  obj <- readobjFile
  specs <- concat <$> mapM testSpecs
             [ spec_parseUpdateData obj bstr bstrFail
             ]
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs,
                  testGroup "Properties" props
                --, testGroup "Golden Tests" goldens
                ])

-- | Config file
updatesFile, updatesFileFail, update_objFile :: FilePath
updatesFile = "src/Bot/files/Test/Request/getUpdates.json"
updatesFileFail = "src/Bot/files/Test/Request/getUpdatesFail.json"
update_objFile = "src/Bot/files/Test/Request/getUpdatesObj.txt"

-- | Read the config JSON file.
readUpdate, readobjFile, readUpdateFail :: IO B.ByteString
readUpdate = B.readFile updatesFile
readUpdateFail = B.readFile updatesFileFail
readobjFile = B.readFile update_objFile