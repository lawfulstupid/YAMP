{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module YAMP.Data.Types where

import YAMP.Data.Parser
import Data.Char (isDigit)

instance MonadPlus m => Parse m Char Char where
   parser = nextToken

-- Matches given character
char :: MonadPlus m => Char -> Parser m Char Char
char c = charThat (== c)

-- Matches any character
anyChar :: MonadPlus m => Parser m Char Char
anyChar = parser

-- Match character that passes a test, useful with Data.Char functions like `isSpace`
charThat :: MonadPlus m => (Char -> Bool) -> Parser m Char Char
charThat test = do
   c <- anyChar
   guard (test c)
   pure c


instance MonadPlus m => Parse m Char String where
   parser = many nextToken

-- Matches given string
string :: MonadPlus m => String -> Parser m Char String
string s = stringThat (== s)

-- Matches any string
anyString :: MonadPlus m => Parser m Char String
anyString = parser

stringThat :: MonadPlus m => (String -> Bool) -> Parser m Char String
stringThat test = do
   s <- anyString
   guard (test s)
   pure s

instance MonadPlus m => Parse m Char Integer where
   parser = peek (charThat isDigit) >> readParser

-- Matches a given integer as a Num
int :: (Num a, Eq a, MonadPlus m) => a -> Parser m Char a
int n = intThat (== n)

-- Matches any integer as a Num
anyInt :: (Num a, MonadPlus m) => Parser m Char a
anyInt = fromInteger <$> parser

intThat :: (Num a, MonadPlus m) => (a -> Bool) -> Parser m Char a
intThat test = do
   n <- anyInt
   guard (test n)
   pure n
