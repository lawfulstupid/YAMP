module YAMP.Control.Combinators where

import YAMP.Data.Parser
import YAMP.Data.Stream

import Data.List (sortOn)

-- Repeat a parsers exactly n times.
exactly :: (Monad m, Alternative m) => Int -> Parser m t a -> Parser m t [a]
exactly n = between (n,n)

-- Repeat a parser between a and b times.
between :: (Monad m, Alternative m) => (Int, Int) -> Parser m t a -> Parser m t [a]
between (a,b) f = do
   xs <- many f
   guard (a <= length xs && length xs <= b)
   return xs

