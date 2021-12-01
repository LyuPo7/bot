import qualified Data.ByteString.Lazy as B
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (testSpecs)

import qualified TestBot.Config as Config
import qualified TestBot.Mode.Mode as BotMode
import qualified TestBot.Parser.Parser as BotParser
import qualified TestBot.Request.Request as BotReq

props :: [TestTree]
props =
  [ testProperty
      "Successful checkConfig for each valid Config"
      Config.prop_checkConfig,
    testProperty
      "Unsuccessful answerMode parse for each text Message \
      \while waiting answer number"
      BotMode.prop_answerModeFail,
    testProperty
      "Successful answerMode parse for each Message \
      \with number 5 in body while waiting answer number"
      BotMode.prop_answerModeSuc,
    testProperty
      "Successful replyMode for /help, /start bot command"
      BotMode.prop_replyModeReplySuc,
    testProperty
      "Successful replyMode for /reply bot command"
      BotMode.prop_replyModeAnswerSuc
  ]

main :: IO ()
main = do
  bstrTele <- readUpdateTele
  bstrFailTele <- readUpdateFailTele
  objTele <- readObjFileTele

  bstrVk <- readUpdateVk
  bstrFailVk <- readUpdateFailVk
  objVk <- readObjFileVk

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

  specs <-
    concat
      <$> mapM
        testSpecs
        [ BotParser.spec_parseData_TeleUpData_UpdateData
            objTele
            bstrTele
            bstrFailTele,
          BotParser.spec_parseData_VkUpData_UpdateData
            objVk
            bstrVk
            bstrFailVk,
          BotParser.spec_parseData_VkPollResp_PollResponse
            objServerVk
            bstrServerVk,
          BotParser.spec_parseData_VkUpUrlResp_UploadUrlResponse
            objUpUrlVk
            bstrUpUrlVk
            bstrUpUrlFailVk,
          BotParser.spec_parseData_VkUpFileResp_UploadFileResponse
            objUpFileVk
            bstrUpFileVk
            bstrUpFileFailVk,
          BotParser.spec_parseData_VkUpObjResp_UploadObjectResponse
            objUpObjVk
            bstrUpObjVk
            bstrUpObjFailVk,
          BotReq.spec_getServer,
          BotReq.spec_getUploadedServer,
          BotReq.spec_sendEchoMessage,
          BotReq.spec_sendHelpMessage,
          BotReq.spec_saveUploadedDoc,
          BotReq.spec_updateMessage,
          BotReq.spec_updateDoc
        ]
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs,
          testGroup "Properties" props
        ]
    )

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
