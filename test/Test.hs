import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Hedgehog
import qualified Data.ByteString.Lazy as B

import TestBot.Config
import qualified TestBot.Tele.Parser as TeleParser
import qualified TestBot.Vk.Parser as VkParser
import TestBot.Tele.Run
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
  bstrTele <- readUpdateTele
  bstrFailTele <- readUpdateFailTele
  objTele <- readobjFileTele

  bstrVk <- readUpdateVk
  bstrFailVk <- readUpdateFailVk
  objVk <- readobjFileVk

  bstrServerVk <- readServerVk
  objServerVk <- readServerObjVk
  
  bstrUpUrlFailVk <- readUpUrlFailVk
  bstrUpUrlVk <- readUpUrlVk
  objUpUrlVk <- readObjUpUrlVk

  bstrUpFileFailVk <- readUpFileFailVk
  bstrUpFileVk <- readUpFileVk
  objUpFileVk <- readObjUpFileVk

  bstrUpObjFailVk <- readUpObjFailVk
  bstrUpObjVk <- readUpObjVk
  objUpObjVk <- readObjUpObjVk

  specs <- concat <$> mapM testSpecs
             [ TeleParser.spec_parseUpdateData objTele bstrTele bstrFailTele,
               VkParser.spec_parseUpdateData objVk bstrVk bstrFailVk,
               VkParser.spec_parsePollResponse objServerVk bstrServerVk,
               VkParser.spec_parseUploadUrl objUpUrlVk bstrUpUrlVk bstrUpUrlFailVk,
               VkParser.spec_parseUploadFile objUpFileVk bstrUpFileVk bstrUpFileFailVk,
               VkParser.spec_parseUploadObject objUpObjVk bstrUpObjVk bstrUpObjFailVk
             ]
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs,
                  testGroup "Properties" props
                --, testGroup "Golden Tests" goldens
                ])

-- | Tele UpdateData file
updatesFileTele, updatesFileFailTele, updateObjFileTele :: FilePath
updatesFileTele = "src/Bot/files/Test/Request/getUpdates.json"
updatesFileFailTele = "src/Bot/files/Test/Request/getUpdatesFail.json"
updateObjFileTele = "src/Bot/files/Test/Request/getUpdatesObj.txt"

-- | Read Tele UpdateData JSON/txt file.
readUpdateTele, readobjFileTele, readUpdateFailTele :: IO B.ByteString
readUpdateTele = B.readFile updatesFileTele
readUpdateFailTele = B.readFile updatesFileFailTele
readobjFileTele = B.readFile updateObjFileTele

-- | Vk UpdateData file
updatesFileVk, updatesFileFailVk, updateObjFileVk :: FilePath
updatesFileVk = "src/Bot/files/Test/Vk/Parser/UpdateData.json"
updatesFileFailVk = "src/Bot/files/Test/Vk/Parser/UpdateDataFail.json"
updateObjFileVk = "src/Bot/files/Test/Vk/Parser/UpdateDataObj.txt"

-- | Read Vk UpdateData JSON/txt file.
readUpdateVk, readobjFileVk, readUpdateFailVk :: IO B.ByteString
readUpdateVk = B.readFile updatesFileVk
readUpdateFailVk = B.readFile updatesFileFailVk
readobjFileVk = B.readFile updateObjFileVk

-- | Vk PollServerResponse file
serverFileVk, serverObjFileVk :: FilePath
serverFileVk = "src/Bot/files/Test/Vk/Parser/PollResponse.json"
serverObjFileVk = "src/Bot/files/Test/Vk/Parser/PollResponseObj.txt"

-- | Read Vk PollServerResponse JSON/txt file.
readServerVk, readServerObjVk :: IO B.ByteString
readServerVk = B.readFile serverFileVk
readServerObjVk = B.readFile serverObjFileVk

-- | Vk UploadUrlResponse file
upUrlFileVk, upUrlFailFileVk, upUrlObjVk :: FilePath
upUrlFileVk = "src/Bot/files/Test/Vk/Parser/UploadUrlResponse.json"
upUrlFailFileVk = "src/Bot/files/Test/Vk/Parser/UploadUrlResponseFail.json"
upUrlObjVk = "src/Bot/files/Test/Vk/Parser/UploadUrlResponseObj.txt"

-- | Read Vk UploadUrlResponse JSON/txt file.
readObjUpUrlVk, readUpUrlFailVk, readUpUrlVk :: IO B.ByteString
readUpUrlVk = B.readFile upUrlFileVk
readUpUrlFailVk = B.readFile upUrlFailFileVk
readObjUpUrlVk = B.readFile upUrlObjVk

-- | Vk UploadFileResponse file
upFileFileVk, upFileFailFileVk, upFileObjVk :: FilePath
upFileFileVk = "src/Bot/files/Test/Vk/Parser/UploadFileResponse.json"
upFileFailFileVk = "src/Bot/files/Test/Vk/Parser/UploadFileResponseFail.json"
upFileObjVk = "src/Bot/files/Test/Vk/Parser/UploadFileResponseObj.txt"

-- | Read Vk UploadFileResponse JSON/txt file.
readObjUpFileVk, readUpFileFailVk, readUpFileVk :: IO B.ByteString
readUpFileVk = B.readFile upFileFileVk
readUpFileFailVk = B.readFile upFileFailFileVk
readObjUpFileVk = B.readFile upFileObjVk

-- | Vk UploadObjectResponse file
upObjFileVk, upObjFailFileVk, upObjObjVk :: FilePath
upObjFileVk = "src/Bot/files/Test/Vk/Parser/UploadObjectResponse.json"
upObjFailFileVk = "src/Bot/files/Test/Vk/Parser/UploadObjectResponseFail.json"
upObjObjVk = "src/Bot/files/Test/Vk/Parser/UploadObjectResponseObj.txt"

-- | Read Vk UploadObjectResponse JSON/txt file.
readObjUpObjVk, readUpObjFailVk, readUpObjVk :: IO B.ByteString
readUpObjVk = B.readFile upObjFileVk
readUpObjFailVk = B.readFile upObjFailFileVk
readObjUpObjVk = B.readFile upObjObjVk