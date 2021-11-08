import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.Hspec (testSpecs)
import Test.Tasty.Hedgehog (testProperty)
import qualified Data.ByteString.Lazy as B

import qualified TestBot.Config as Cnfg
import qualified TestBot.Tele.Parser as TeleParser
import qualified TestBot.Vk.Parser as VkParser
import qualified TestBot.Vk.Attach as VkAttach
import qualified TestBot.Vk.Document as VkDoc
import qualified TestBot.Vk.Request as VkReq
import qualified TestBot.Tele.Run as TeleRun
import qualified TestBot.Vk.Run as VkRun

props :: [TestTree]
props = [
  testProperty "Tele: Successful checkConfig for each valid Config"
    Cnfg.prop_checkConfig,
  testProperty "Tele: Unsuccessful answerMode parse for each text Message \
               \while waiting answer number"
               TeleRun.prop_answerModeFail,
  testProperty "Tele: Successful answerMode parse for each Message \
               \with number 5 in body while waiting answer number" 
               TeleRun.prop_answerModeSuc,
  testProperty "Tele: Successful replyMode for /help, /start bot command"
    TeleRun.prop_replyModeReplySuc,
  testProperty "Tele: Successful replyMode for /reply bot command"
    TeleRun.prop_replyModeAnswerSuc,
  testProperty "Tele: Successful replyMode for message without bot command"
    TeleRun.prop_replyModeOrdinarySuc,
  testProperty "Vk: Unsuccessful answerMode parse for each text Message \
               \while waiting answer number"
               VkRun.prop_answerModeFail,
  testProperty "Vk: Successful answerMode parse for each Message \
               \with number 5 in body while waiting answer number"
               VkRun.prop_answerModeSuc,
  testProperty "Vk: Successful replyMode for /help bot command"
    VkRun.prop_replyModeReplySuc,
  testProperty "Vk: Successful replyMode for /reply bot command"
    VkRun.prop_replyModeAnswerSuc,
  testProperty "Vk: Successful replyMode for message without bot command"
    VkRun.prop_replyModeOrdinarySuc
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
               VkParser.spec_parseUploadObject objUpObjVk bstrUpObjVk bstrUpObjFailVk,
               VkAttach.spec_updateAttachment,
               VkAttach.spec_updateAttachments,
               VkReq.spec_returnStickerId,
               VkReq.spec_attachmentToString,
               VkReq.spec_attachmentsToQuery,
               VkReq.spec_createHelpMessage,
               VkReq.spec_createEchoMessage,
               VkReq.spec_createRepeatMessage,
               VkReq.spec_createServerQuery,
               VkReq.spec_geoToLatLong,
               VkDoc.spec_updateDoc
             ]
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs,
                  testGroup "Properties" props
                --, testGroup "Golden Tests" goldens
                ])

-- | Tele UpdateData file
updatesFileTele, updatesFileFailTele, updateObjFileTele :: FilePath
updatesFileTele = "data/Test/Request/getUpdates.json"
updatesFileFailTele = "data/Test/Request/getUpdatesFail.json"
updateObjFileTele = "data/Test/Request/getUpdatesObj.txt"

-- | Read Tele UpdateData JSON/txt file.
readUpdateTele, readObjFileTele, readUpdateFailTele :: IO B.ByteString
readUpdateTele = B.readFile updatesFileTele
readUpdateFailTele = B.readFile updatesFileFailTele
readObjFileTele = B.readFile updateObjFileTele

-- | Vk UpdateData file
updatesFileVk, updatesFileFailVk, updateObjFileVk :: FilePath
updatesFileVk = "data/Test/Vk/Parser/UpdateData.json"
updatesFileFailVk = "data/Test/Vk/Parser/UpdateDataFail.json"
updateObjFileVk = "data/Test/Vk/Parser/UpdateDataObj.txt"

-- | Read Vk UpdateData JSON/txt file.
readUpdateVk, readObjFileVk, readUpdateFailVk :: IO B.ByteString
readUpdateVk = B.readFile updatesFileVk
readUpdateFailVk = B.readFile updatesFileFailVk
readObjFileVk = B.readFile updateObjFileVk

-- | Vk PollServerResponse file
serverFileVk, serverObjFileVk :: FilePath
serverFileVk = "data/Test/Vk/Parser/PollResponse.json"
serverObjFileVk = "data/Test/Vk/Parser/PollResponseObj.txt"

-- | Read Vk PollServerResponse JSON/txt file.
readServerVk, readServerObjVk :: IO B.ByteString
readServerVk = B.readFile serverFileVk
readServerObjVk = B.readFile serverObjFileVk

-- | Vk UploadUrlResponse file
upUrlFileVk, upUrlFailFileVk, upUrlObjVk :: FilePath
upUrlFileVk = "data/Test/Vk/Parser/UploadUrlResponse.json"
upUrlFailFileVk = "data/Test/Vk/Parser/UploadUrlResponseFail.json"
upUrlObjVk = "data/Test/Vk/Parser/UploadUrlResponseObj.txt"

-- | Read Vk UploadUrlResponse JSON/txt file.
readObjUpUrlVk, readUpUrlFailVk, readUpUrlVk :: IO B.ByteString
readUpUrlVk = B.readFile upUrlFileVk
readUpUrlFailVk = B.readFile upUrlFailFileVk
readObjUpUrlVk = B.readFile upUrlObjVk

-- | Vk UploadFileResponse file
upFileFileVk, upFileFailFileVk, upFileObjVk :: FilePath
upFileFileVk = "data/Test/Vk/Parser/UploadFileResponse.json"
upFileFailFileVk = "data/Test/Vk/Parser/UploadFileResponseFail.json"
upFileObjVk = "data/Test/Vk/Parser/UploadFileResponseObj.txt"

-- | Read Vk UploadFileResponse JSON/txt file.
readObjUpFileVk, readUpFileFailVk, readUpFileVk :: IO B.ByteString
readUpFileVk = B.readFile upFileFileVk
readUpFileFailVk = B.readFile upFileFailFileVk
readObjUpFileVk = B.readFile upFileObjVk

-- | Vk UploadObjectResponse file
upObjFileVk, upObjFailFileVk, upObjObjVk :: FilePath
upObjFileVk = "data/Test/Vk/Parser/UploadObjectResponse.json"
upObjFailFileVk = "data/Test/Vk/Parser/UploadObjectResponseFail.json"
upObjObjVk = "data/Test/Vk/Parser/UploadObjectResponseObj.txt"

-- | Read Vk UploadObjectResponse JSON/txt file.
readObjUpObjVk, readUpObjFailVk, readUpObjVk :: IO B.ByteString
readUpObjVk = B.readFile upObjFileVk
readUpObjFailVk = B.readFile upObjFailFileVk
readObjUpObjVk = B.readFile upObjObjVk