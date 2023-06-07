module YAMP.Control.Combinators where

import YAMP.Data.Parser
import YAMP.Data.Stream

import Data.List (sortOn)

-- Repeat a parser exactly n times.
exactly :: (Monad m, Alternative m) => Int -> Parser m t a -> Parser m t [a]
exactly n = between (n,n)

-- Repeat a parser between a and b times.
between :: (Monad m, Alternative m) => (Int, Int) -> Parser m t a -> Parser m t [a]
between (a,b) f = do
   xs <- many f
   guard (a <= length xs && length xs <= b)
   return xs

-- Matches the given value
-- E.g. match 'x' or match "foobar"
match :: (Parse t a, Eq a, MonadPlus m) => a -> Parser m t a
match t = do
   x <- parser
   guard (t == x)
   pure x

-- Matches any of the given values
-- E.g. matchAny ["foo", "bar"]
matchAny :: (Parse t a, Eq a, MonadPlus m, Foldable f) => f a -> Parser m t a
matchAny = foldMap match

