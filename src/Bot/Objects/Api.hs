{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Objects.Api where

data Api = Tele
         | Vk
         | InvalidApi
         deriving (Show, Read, Ord, Eq)