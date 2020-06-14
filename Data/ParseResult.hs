module YAMP.Data.ParseResult where

import Data.Bifunctor

data ParseResult s a = Result a s
   deriving (Show)

instance Functor (ParseResult s) where
   fmap f (Result x s) = Result (f x) s

instance Bifunctor ParseResult where
   bimap f g (Result x s) = Result (g x) (f s)

result :: (a,s) -> ParseResult s a
result = uncurry Result