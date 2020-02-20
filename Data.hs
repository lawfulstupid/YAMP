{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AbLib.Parser.Data where

import Prelude hiding (id, (.))

import Control.Monad
import Control.Applicative
import Control.Category
import Control.Arrow

import Data.List (stripPrefix, nub)
import Data.Maybe (maybeToList)

import AbLib.Control.Alias ((<<))

--------------------------------------------------------------------------------

data Parser c s t = Parser (s -> [(t, [c])])
-- type ParseS s t = s -> [(t, s)]

instance Category (Parser c) where
   -- id  :: cat a a
   id = Parser $ \s -> [(s, [])]
   -- (.) :: cat b c -> cat a b -> cat a c
   Parser g . Parser f = Parser $ \s -> [ (y,r) | (x,r) <- f s, (y,r'::[b]) <- g x ]

-- apply :: Parser s t -> ParseS s t
-- apply (Parser p) = p

-- parse :: (Parse s t, Monoid s) => s -> t
-- parse = maybe parseErr id . parseMaybe
   -- where parseErr = errorWithoutStackTrace "no parse"

-- parseMaybe :: (Parse s a, Monoid s) => s -> Maybe a
-- parseMaybe s = case apply parser s of
   -- [(x, mempty)] -> Just x
   -- _ -> Nothing

-- class Parse s a where
   -- parser :: Parser s a

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

