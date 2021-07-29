{-# LANGUAGE OverloadedStrings #-}

module Tele.Requests where

import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Types.Status (statusCode)

import Config
import Logger
import LoggerIO
import Settings (apiTele, config, getHost)
import Tele.Types

makeRequest :: Logger.Handle -> TeleRequest -> RequestOptions -> IO B.ByteString
makeRequest logh teleRequest options = do
          manager <- newManager tlsManagerSettings
          token <- fmap botToken config
          let host = BC.unpack $ getHost apiTele
          let method = BC.unpack $ getRequest teleRequest
          let api = concat [host, unpack token, method]
          initialRequest <- parseRequest api
          let request =
                        initialRequest
                                  { method = "POST"
                                  , requestBody =
                                              RequestBodyLBS $ encode options
                                  , requestHeaders =
                                              [ ( "Content-Type"
                                                , "application/json; charset=utf-8")
                                              ]
                                  }
          response <- httpLbs request manager
          logDebug logh $
                    "The status code was: " ++
                    show (statusCode $ responseStatus response)
          return $ responseBody response

getUpdate :: Logger.Handle -> Maybe Integer -> IO L8.ByteString
getUpdate logh updateId
  -- Get JSON data
 = do
          let updateOptions =
                        GetUpdates
                                  { updates_offset = updateId
                                  , updates_limit = Nothing
                                  , updates_timeout = Nothing
                                  , updates_allowedUpdates = Just ["message"]
                                  }
          makeRequest logh getUpdates updateOptions

sendTextMessage :: Logger.Handle -> Integer -> Text -> IO ()
sendTextMessage logh chatId text = do
          let message =
                        SendMessage
                                  { sendMes_chatId = chatId
                                  , sendMes_text = text
                                  , sendMes_disableNotification = Nothing
                                  , sendMes_replyToMessageId = Nothing
                                  }
          _ <- makeRequest logh sendMessage message
          logInfo logh $
                    "Message with text: " ++
                    show text ++ " was sended to chat with id: " ++ show chatId

sendEchoMessage :: Logger.Handle -> Integer -> Integer -> IO ()
sendEchoMessage logh chatId messageId = do
          let message =
                        CopyMessage
                                  { copwMes_chatId = chatId
                                  , copwMes_fromChatId = chatId
                                  , copwMes_messageId = messageId
                                  }
          _ <- makeRequest logh copyMessage message
          logInfo logh $
                    "Echo-Message with id: " ++
                    show messageId ++
                    " was forwarded to chat with id: " ++ show chatId

sendNEchoMessage :: Logger.Handle -> Integer -> Integer -> Integer -> IO ()
sendNEchoMessage logh chatId messageId 0 =
          logInfo logh "Echo-Messages were sended."
sendNEchoMessage logh chatId messageId n = do
          sendEchoMessage logh chatId messageId
          sendNEchoMessage logh chatId messageId (n - 1)

sendQueryNumber :: Logger.Handle -> Integer -> Text -> IO L8.ByteString
sendQueryNumber logh chatId question = do
          let b1 =
                        InlineKeyboardButton
                                  { inlineButton_text = "1"
                                  , inlineButton_callbackData = Just "Pressed 1"
                                  }
          let b2 =
                        InlineKeyboardButton
                                  { inlineButton_text = "2"
                                  , inlineButton_callbackData = Just "Pressed 2"
                                  }
          let b3 =
                        InlineKeyboardButton
                                  { inlineButton_text = "3"
                                  , inlineButton_callbackData = Just "Pressed 3"
                                  }
          let b4 =
                        InlineKeyboardButton
                                  { inlineButton_text = "4"
                                  , inlineButton_callbackData = Just "Pressed 4"
                                  }
          let b5 =
                        InlineKeyboardButton
                                  { inlineButton_text = "5"
                                  , inlineButton_callbackData = Just "Pressed 5"
                                  }
          let markupIn =
                        InlineKeyboardMarkup
                                  { inlineMarkup_keyboard =
                                              [[b1, b2, b3, b4, b5]]
                                  , inlineMarkup_oneTimeKeyboard = Just True
                                  , inlineMarkup_resizeKeyboard = Just True
                                  }
          let query =
                        QueryMessage
                                  { sendQue_chatId = chatId
                                  , sendQue_text = question
                                  , sendQue_disableNotification = Nothing
                                  , sendQue_replyToMessageId = Nothing
                                  , sendQue_replyMarkup = Just markupIn
                                  }
          logInfo logh $ "Question was sended to chat with id: " ++ show chatId
          makeRequest logh sendMessage query
