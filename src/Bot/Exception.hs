{-# LANGUAGE DeriveAnyClass #-}

module Bot.Exception where

import Control.Exception (Exception)

data BotError = ParseRequestError String
               | ParseConfigError String
               | ConnectionError Int
               | GetServerError
               | UploadedServerError
               | UploadedDocError
               | HelpMessageError
               | StartMessageError
               | ApiObjectError String
               | ApiMethodError
               | InvalidApiError String
               | DbError String
               | Default String
               deriving (Exception, Eq)

instance Show BotError where
  show (ParseRequestError err) = "Parse error: " 
                               ++ err
  show (ParseConfigError err) = "Error while parsing config file \
                                \(config.json): " ++ err
  show (ConnectionError code) = "Unsuccessful request to api with code: "
                              ++ show code
  show GetServerError = "Incorrect API for getServer function!"
  show UploadedServerError = "Incorrect API for uploading files!"
  show UploadedDocError = "Incorrect API for uploading documents!"
  show HelpMessageError = "This API hasn't '/help' message"
  show StartMessageError = "This API hasn't '/start' message"
  show (ApiObjectError err) = "Incorrect Object for this Api: " ++ err
  show ApiMethodError = "Incorrect Method for this Api!"
  show (InvalidApiError invalidApi) = "This Api: "
                                    ++ invalidApi
                                    ++ " isn't supported!"
  show (Default _) = "Default error"
  show (DbError err) = "Db error: " ++ err