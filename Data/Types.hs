{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module YAMP.Data.Types where

import YAMP.Data.Parser
import YAMP.Control.Combinators

instance MonadPlus m => Parse m Char Char where
   parser = nextToken

-- Matches given character
char :: MonadPlus m => Char -> Parser m Char Char
char = match

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
string = match

-- Matches any string
anyString :: MonadPlus m => Parser m Char String
anyString = parser


instance MonadPlus m => Parse m Char Integer where
   parser = readParser

-- Matches a given integer as a Num
int :: (MonadPlus m, Integral a, Num b) => a -> Parser m Char b
int n = fromInteger <$> match (fromIntegral n)

-- Matches any integer as a Num
anyInt :: (MonadPlus m, Num a) => Parser m Char a
anyInt = fromInteger <$> parser
