{-# LANGUAGE DeriveAnyClass #-}

module Bot.Exception where

import Control.Exception (Exception)

data BotError = ParseRequestError String
               | ParseConfigError String
               | ConnectionError Int
               | DbError String
               | Default String
               deriving (Exception, Eq)

instance Show BotError where
    show (ParseRequestError err) = "Parse error: " ++ err
    show (ParseConfigError err) = "Error while parsing config file (config.json): " ++ err
    show (ConnectionError code) = "Unsuccessfull request to api with code: " ++ show code
    show (Default _) = "Default error"
    show (DbError err) = "Db error: " ++ err

type ThrowsError = Either BotError