module Bot.Exception where

import Control.Monad.Except ()

data BotError = ParseError String
               | DbError String
               | Default String

instance Show BotError where
    show (ParseError err) = "Parse error at " ++ err
    show (Default _) = "Default error"
    show (DbError err) = "Db error " ++ err

type ThrowsError = Either BotError