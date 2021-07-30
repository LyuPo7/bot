{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (eitherDecode)
import System.IO.Error ()
   
import qualified Bot.Logger as BL
import Bot.Vk.Parser.Data (PollResponse(..), UpdateData(..), UploadUrlResponse(..), UploadFileResponse(..), UploadObjectResponse(..))

{-- | PollResponse parser --}
parsePollResponse :: BL.Handle -> IO L8.ByteString -> IO PollResponse
parsePollResponse logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String PollResponse)
  case d of
    Left err -> do
      BL.logError logh $ "Couldn't parse poll response: " ++ err
      fail err --main = toTry `catchIOError` handler
    Right ps -> do
      BL.logDebug logh "Poll response was successfully parsed."
      return ps

{-- | UpdateData parser --}
parseUpdateData :: BL.Handle -> IO L8.ByteString -> IO UpdateData
parseUpdateData logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String UpdateData)
  case d of
    Left err -> do
      BL.logError logh $ "Couldn't parse UpdateData: " ++ err
      return UpdateData {ts = "0", updates = []}
    Right ps -> do
      BL.logDebug logh "UpdateData was successfully parsed."
      return ps
  
{-- | UploadUrlResponse parser --}
parseUploadUrl :: BL.Handle -> IO L8.ByteString -> IO UploadUrlResponse
parseUploadUrl logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String UploadUrlResponse)
  case d of
    Left err -> do
      BL.logError logh $ "Couldn't parse UploadUrlResponse: " ++ err
      return UploadUrlResponse {upUrlResponse_response = Nothing}
    Right ps -> do
      BL.logDebug logh "UploadUrlResponse was successfully parsed."
      return ps

{-- | UploadFileResponse parser --}
parseUploadFile :: BL.Handle -> IO L8.ByteString -> IO UploadFileResponse
parseUploadFile logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String UploadFileResponse)
  case d of
    Left err -> do
      BL.logError logh $ "Couldn't parse parseUploadFile: " ++ err
      return UploadFileResponse {upFileResponse_file = Nothing}
    Right ps -> do
      BL.logDebug logh "UploadFileResponse was successfully parsed."
      return ps

{-- | UploadObjectResponse parser --}
parseUploadObject :: BL.Handle -> IO L8.ByteString -> IO UploadObjectResponse
parseUploadObject logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String UploadObjectResponse)
  case d of
    Left err -> do
      BL.logError logh $ "Couldn't parse parseUploadObject: " ++ err
      return UploadObjectResponse {upObjResponse_response = []}
    Right ps -> do
      BL.logDebug logh "UploadObjectResponse was successfully parsed."
      return ps