module Bot.Parser.Parser where

import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T

import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp
import qualified Bot.DB.DB as BotDB
import qualified Bot.Logger.Logger as Logger

class (FromJSON a) => Parser a where
  parseData ::
    Monad m =>
    BotDB.Handle m ->
    L8.ByteString ->
    m (Either Text a)
  parseData handle response = do
    let logH = BotDB.hLogger handle
        d = eitherDecode response
    case d of
      Left err -> do
        Logger.logError logH $
          "Couldn't parse Data: "
            <> T.pack err
        return $ Left $ T.pack err
      Right ps -> do
        Logger.logDebug logH "Data was successfully parsed."
        return $ Right ps

instance Parser TeleUpData.UpdateData

instance Parser VkPollResp.PollResponse

instance Parser VkUpData.UpdateData

instance Parser VkUpUrlResp.UploadUrlResponse

instance Parser VkUpFileResp.UploadFileResponse

instance Parser VkUpObjResp.UploadObjectResponse
