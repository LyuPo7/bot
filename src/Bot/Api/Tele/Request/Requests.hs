module Bot.Api.Tele.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Control.Exception as Exc
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Data.Aeson (encode)
import Network.HTTP.Client (Request(..), RequestBody(..), 
                            newManager, parseRequest, httpLbs,
                            responseStatus, responseBody)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Settings as Settings
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Method as BotMethod
import qualified Bot.Objects.RequestOptions as BotReqOptions
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Tele.Objects.RequestOptions as TeleReqOptions
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Keyboard as TeleKeyboard
import qualified Bot.Api.Tele.Objects.Button as TeleButton
import qualified Bot.Util as BotUtil

withHandleIO :: Logger.Handle IO -> BotDBQ.Handle IO -> BotParser.Handle IO ->
                Settings.Config -> (BotReq.Handle IO -> IO a) -> IO a
withHandleIO logger dbH parserH config f = do
  let handle = BotReq.Handle {
    BotReq.hLogger = logger,
    BotReq.hDb = dbH,
    BotReq.hParser = parserH,
    BotReq.cReq = config,

    BotReq.makeRequest = makeRequest parserH,
    BotReq.setUploadedServer = \_ -> return Nothing,
    BotReq.setUploadedDoc = \_ -> return Nothing,
    BotReq.setGetServer = return Nothing,
    BotReq.setGetUpdate = setGetUpdate parserH,
    BotReq.setEchoMessage = setEchoMessage parserH,
    BotReq.setHelpMessage = setHelpMessage parserH,
    BotReq.setStartMessage = setStartMessage parserH,
    BotReq.setKeyboardMessage = setKeyboardMessage parserH,
    BotReq.setCommands = setCommands parserH,

    BotReq.downloadDoc = \_ _ -> return Nothing,
    BotReq.extractDoc = \_ -> return Nothing,
    BotReq.changeMessage = \message _-> return message,
    BotReq.changeDoc = \doc _ -> return doc
  }
  f handle

setGetUpdate :: Monad m => BotParser.Handle m -> BotUpdate.Update ->
                m (BotMethod.Method, BotReqOptions.RequestOptions)
setGetUpdate _ botUpdate = do
  let updateId = BotUpdate.teleUpdate botUpdate
      reqOptions = BotReqOptions.TeleReqOptions $
                   TeleReqOptions.createGetUpdates updateId
      apiMethod = BotMethod.TeleMethod TeleMethod.getUpdates
  return (apiMethod, reqOptions)

setEchoMessage :: Monad m => BotParser.Handle m -> BotMessage.Message ->
                  m (BotMethod.Method, BotReqOptions.RequestOptions)
setEchoMessage _ botMessage = do
  let message = BotMessage.teleMessage botMessage
      chatId = TeleChat.id $ TeleMessage.chat message
      messageId = TeleMessage.message_id message
      reqOptions = BotReqOptions.TeleReqOptions
        (TeleReqOptions.createEchoMessage chatId messageId)
      apiMethod = BotMethod.TeleMethod TeleMethod.copyMessage
  return (apiMethod, reqOptions)

setHelpMessage :: Monad m => BotParser.Handle m -> BotMessage.Message ->
                  BotSynonyms.Description ->
                  m (BotMethod.Method, BotReqOptions.RequestOptions)
setHelpMessage _ botMessage description = do
  let message = BotMessage.teleMessage botMessage
      chatId = TeleChat.id $ TeleMessage.chat message
      reqOptions = BotReqOptions.TeleReqOptions 
        (TeleReqOptions.createTextMessage chatId description)
      apiMethod = BotMethod.TeleMethod TeleMethod.sendMessage
  return (apiMethod, reqOptions)

setStartMessage :: Monad m => BotParser.Handle m ->
                   BotMessage.Message ->
                   Text ->
                   m (Maybe(BotMethod.Method, BotReqOptions.RequestOptions))
setStartMessage _ botMessage startText = do
  let message = BotMessage.teleMessage botMessage
      chatId = TeleChat.id $ TeleMessage.chat message
      reqOptions = BotReqOptions.TeleReqOptions
        (TeleReqOptions.createTextMessage chatId startText)
      apiMethod = BotMethod.TeleMethod TeleMethod.sendMessage
  return $ Just (apiMethod, reqOptions)

setKeyboardMessage :: Monad m => BotParser.Handle m ->
                      BotMessage.Message ->
                     [BotButton.Button] ->
                      Text ->
                      m (BotMethod.Method, BotReqOptions.RequestOptions)
setKeyboardMessage _ botMessage buttons question = do
  let message = BotMessage.teleMessage botMessage
      chatId = TeleChat.id $ TeleMessage.chat message
      [b1, b2, b3, b4, b5] = buttons
      b1Tele = TeleButton.createButton
        (BotButton.text b1) (BotButton.description b1)
      b2Tele = TeleButton.createButton
        (BotButton.text b2) (BotButton.description b2)
      b3Tele = TeleButton.createButton
        (BotButton.text b3) (BotButton.description b3)
      b4Tele = TeleButton.createButton
        (BotButton.text b4) (BotButton.description b4)
      b5Tele = TeleButton.createButton
        (BotButton.text b5) (BotButton.description b5)
      keyboard = TeleKeyboard.createKeyboard
        [[b1Tele, b2Tele, b3Tele, b4Tele, b5Tele]]
      apiMethod = BotMethod.TeleMethod TeleMethod.sendMessage
      reqOptions = BotReqOptions.TeleReqOptions
        (TeleReqOptions.createKeyboardMessage chatId question keyboard)
  return (apiMethod, reqOptions)

setCommands :: Monad m => BotParser.Handle m ->
               m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions))
setCommands _ = do
  let commands = TeleReqOptions.createCommands
      apiMethod = BotMethod.TeleMethod TeleMethod.setCommands
      reqOptions = BotReqOptions.TeleReqOptions commands
  return $ Just (apiMethod, reqOptions)

makeRequest :: BotParser.Handle IO -> BotMethod.Method ->
               BotReqOptions.RequestOptions -> IO B.ByteString
makeRequest handle botMethod botOptions = do
  let logH = BotParser.hLogger handle
      config = BotParser.cParser handle
      apiMethod = BotMethod.teleMethod botMethod
      options = BotReqOptions.teleReqOptions botOptions
      token = Settings.botToken config
      hostApi = Settings.getHost Settings.apiTele
      methodApi = TeleMethod.getMethod apiMethod
      api = T.concat [hostApi, token, methodApi]
  manager <- newManager tlsManagerSettings
  Logger.logDebug logH $ "Request method: " <> BotUtil.convertValue apiMethod
  Logger.logDebug logH $ "Request options: " <> BotUtil.convertValue options
  initialRequest <- parseRequest $ T.unpack api
  let request = initialRequest { 
    method = "POST",
    requestBody = RequestBodyLBS $ encode options,
    requestHeaders = [ ( "Content-Type",
                         "application/json; charset=utf-8")
                      ]
  }
  response <- httpLbs request manager
  let codeResp = statusCode $ responseStatus response
  if codeResp == 200
    then do
      Logger.logInfo logH "Successful request to api."
      return $ responseBody response
    else do
      Logger.logWarning logH $ "Unsuccessful request to api with code: "
        <> BotUtil.convertValue codeResp
      B.putStr $ responseBody response
      Exc.throwIO $ E.ConnectionError codeResp