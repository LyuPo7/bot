{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8         as BS
import           Data.Text                
import           Data.Aeson.Lens               
import           Control.Lens                   ( preview )
import qualified Data.Text.IO                  as TIO
import Data.Aeson.Types
import Tele.Types
import Data.Aeson


getUpdatesTele :: IO BS.ByteString
getUpdatesTele = do
  res2 <- httpBS "https://api.telegram.org/bot1688182700:AAHcaPS9PZ9pS29T5TOLXNeC8BqDvCckZVg/getUpdates?offset=10?allowed_updates=messages"
  res <- httpBS "https://api.telegram.org/bot1688182700:AAHcaPS9PZ9pS29T5TOLXNeC8BqDvCckZVg/getUpdates"
  return (getResponseBody res2)

getOk :: BS.ByteString -> Maybe Bool
getOk = preview (key "ok" . _Bool)

getText :: BS.ByteString -> Maybe Text
getText = preview (key "result" . key "0" . key "message" . key "text" . _String)

getArray :: BS.ByteString -> Maybe Array
getArray = preview (key "result" . _Array)

main :: IO ()
main = do
  json <- getUpdatesTele
  
  case getOk json of
    Nothing   -> TIO.putStrLn "Could not find the status :("
    Just status -> TIO.putStrLn $ "The status is " <> (pack $ show $ status) <> "."
  case getText json of
    Nothing   -> TIO.putStrLn "Could not find the message :("
    Just text -> TIO.putStrLn $ "The message is " <> (pack $ show $ text) <> "."
  case getArray json of
    Nothing   -> TIO.putStrLn ":("
    --Just array -> eitherDecode array :: IO (Either String [Update])
