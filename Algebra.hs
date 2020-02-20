
module AbLib.Parser.Algebra where

import AbLib.Parser.Data

import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))

instance Semigroup (Parser s a) where
   (<>) = (<|>)
   
instance Monoid (Parser s a) where
   mempty = Parser $ const []

instance Functor (Parser s) where
   fmap f (Parser p) = Parser $ \ s -> 
      [ (f x, r) | (x, r) <- p s ]

instance Applicative (Parser s) where
   pure x = Parser $ \ s -> [(x, s)]
   f <*> p = Parser $ \ s ->
      [ (g x, r') | (x, r) <- apply p s, (g, r') <- apply f r ]
      
instance Alternative (Parser s) where
   empty = mempty
   p <|> q = Parser $ \ s -> apply p s ++ apply q s
   {- Default definitions for `many` and `some` didn't halt. -}
   many p = pure [] <|> some p   -- empty list OR at least one
   some p = do                   -- one or more
      h <- p                     -- list head
      t <- many p                -- zero or more tail elements
      return (h:t)               -- stick the list together

{- Repeat a parsers exactly n times -}
exactly :: Int -> Parser s a -> Parser s [a]
exactly n = sequence . replicate n

instance Monad (Parser s) where
   fail _ = empty
   p >>= f = Parser $ \ s -> do 
      (x,r) <- apply p s
      apply (f x) r

instance MonadPlus (Parser s)
   {- grants access to mfilter et al. -}

instance Category Parser where
   id = Parser $ \ s -> [(s, s)]
   f . g = do
      x <- g
      apply f x
   (.) :: Parser b c -> Parser a b -> Parser a c
   f . g = Parser $ \ s -> do
      (x,r) <- apply g s
      (y,t) <- apply f x
      return (y,r)

f . id  =  f  -- (right identity)
id . f  =  f  -- (left identity)
f . (g . h)  =  (f . g) . h  -- (associativity)

(.) :: Parser b c -> Parser a b -> Parser a c


f . id ?= f
id     :: Parser a b
f      :: Parser b c
f . id :: Parser a c
a === b

f . id = Parser $ \ (s::a) -> do
      (x::a, r::a) <- apply id (s::a)
      (y::c, t::a) <- apply f  (x::a)
      return (y::c, r::a)