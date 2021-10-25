{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Aeson (eitherDecode)
import Data.Text (Text)

import Bot.Vk.Parser.ParserSpec (Handle(..))
import qualified Bot.Logger as Logger
import Bot.Vk.Parser.Data

withHandleIO :: Logger.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger f = do
  let handle = Handle logger
  f handle

{-- | PollResponse parser --}
parsePollResponse :: Monad m => Handle m -> L8.ByteString ->
                     m (Either String PollResponse)
parsePollResponse handle response = do
  let logh = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: Either String PollResponse
  case d of
    Left err -> do
      Logger.logError logh $ "Couldn't parse poll response: "
        <> T.pack err
      return d
    Right _ -> do
      Logger.logDebug logh "Poll response was successfully parsed."
      return d

{-- | PollResponse parser --}
parsePollResponseText :: Monad m => Handle m -> L8.ByteString ->
                         m (Either Text PollResponseText)
parsePollResponseText handle response = do
  let logh = hLogger handle
      d = eitherDecode response :: Either String PollResponseText
  case d of
    Left err -> do
      Logger.logError logh $ "Couldn't parse poll-text response: "
        <> T.pack err
      return $ Left $ T.pack err
    Right pollResponse -> do
      Logger.logDebug logh "Poll-text response was successfully parsed."
      return $ Right pollResponse

{-- | UpdateData parser --}
parseUpdateData :: Monad m => Handle m -> L8.ByteString -> m UpdateData
parseUpdateData handle response = do
  let logh = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: Either String UpdateData
  case d of
    Left err -> do
      Logger.logError logh $ "Couldn't parse UpdateData: " <> T.pack err
      return UpdateData {ts = "0", updates = []}
    Right ps -> do
      Logger.logDebug logh "UpdateData was successfully parsed."
      return ps
  
{-- | UploadUrlResponse parser --}
parseUploadUrl :: Monad m => Handle m -> L8.ByteString -> m UploadUrlResponse
parseUploadUrl handle response = do
  let logh = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: Either String UploadUrlResponse
  case d of
    Left err -> do
      Logger.logError logh $ "Couldn't parse UploadUrlResponse: " <> T.pack err
      return UploadUrlResponse {
        upUrlResponse_response = Nothing
      }
    Right ps -> do
      Logger.logDebug logh "UploadUrlResponse was successfully parsed."
      return ps

{-- | UploadFileResponse parser --}
parseUploadFile :: Monad m => Handle m -> L8.ByteString -> m UploadFileResponse
parseUploadFile handle response = do
  let logh = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: Either String UploadFileResponse
  case d of
    Left err -> do
      Logger.logError logh $ "Couldn't parse parseUploadFile: "
        <> T.pack err
      return UploadFileResponse {upFileResponse_file = Nothing}
    Right ps -> do
      Logger.logDebug logh "UploadFileResponse was successfully parsed."
      return ps

{-- | UploadObjectResponse parser --}
parseUploadObject :: Monad m => Handle m -> L8.ByteString ->
                     m UploadObjectResponse
parseUploadObject handle response = do
  let logh = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: Either String UploadObjectResponse
  case d of
    Left err -> do
      Logger.logError logh $ "Couldn't parse parseUploadObject: "
        <> T.pack err
      return UploadObjectResponse {
        upObjResponse_response = []
      }
    Right ps -> do
      Logger.logDebug logh "UploadObjectResponse was successfully parsed."
      return ps