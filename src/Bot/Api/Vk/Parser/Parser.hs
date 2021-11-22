module Bot.Api.Vk.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Aeson (eitherDecode)
import Data.Text (Text)

import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp

parsePollResponse :: Monad m => BotParser.Handle m -> L8.ByteString ->
                         m (Either Text VkPollResp.PollResponse)
parsePollResponse handle resp = do
  let logH = BotParser.hLogger handle
      d = eitherDecode resp :: Either String VkPollResp.PollResponse
  case d of
    Left err -> do
      Logger.logError logH $ "Couldn't parse poll-text response: "
        <> T.pack err
      return $ Left $ T.pack err
    Right pollResponse -> do
      Logger.logDebug logH "Poll-text response was successfully parsed."
      return $ Right pollResponse

parseUpdateData :: Monad m => BotParser.Handle m ->
                   L8.ByteString -> m VkUpData.UpdateData
parseUpdateData handle resp = do
  let logH = BotParser.hLogger handle
      d = eitherDecode resp :: Either String VkUpData.UpdateData
  case d of
    Left err -> do
      Logger.logDebug logH $ T.pack $ L8.unpack resp
      Logger.logError logH $ "Couldn't parse UpdateData: " <> T.pack err
      return VkUpData.UpdateData {
        VkUpData.ts = "0",
        VkUpData.updates = []
      }
    Right ps -> do
      Logger.logDebug logH "UpdateData was successfully parsed."
      return ps
  
parseUploadUrl :: Monad m => BotParser.Handle m -> L8.ByteString ->
                  m VkUpUrlResp.UploadUrlResponse
parseUploadUrl handle resp = do
  let logH = BotParser.hLogger handle
      d = eitherDecode resp :: Either String VkUpUrlResp.UploadUrlResponse
  case d of
    Left err -> do
      Logger.logError logH $ "Couldn't parse UploadUrlResponse: " <> T.pack err
      return $ VkUpUrlResp.UploadUrlResponse Nothing
    Right ps -> do
      Logger.logDebug logH "UploadUrlResponse was successfully parsed."
      return ps

parseUploadFile :: Monad m => BotParser.Handle m -> L8.ByteString ->
                   m VkUpFileResp.UploadFileResponse
parseUploadFile handle resp = do
  let logH = BotParser.hLogger handle
      d = eitherDecode resp :: Either String VkUpFileResp.UploadFileResponse
  case d of
    Left err -> do
      Logger.logError logH $ "Couldn't parse parseUploadFile: "
        <> T.pack err
      return $ VkUpFileResp.UploadFileResponse Nothing
    Right ps -> do
      Logger.logDebug logH "UploadFileResponse was successfully parsed."
      return ps

parseUploadObject :: Monad m => BotParser.Handle m -> L8.ByteString ->
                     m VkUpObjResp.UploadObjectResponse
parseUploadObject handle resp = do
  let logH = BotParser.hLogger handle
      d = eitherDecode resp :: Either String VkUpObjResp.UploadObjectResponse
  case d of
    Left err -> do
      Logger.logError logH $ "Couldn't parse parseUploadObject: "
        <> T.pack err
      return $ VkUpObjResp.UploadObjectResponse []
    Right ps -> do
      Logger.logDebug logH "UploadObjectResponse was successfully parsed."
      return ps