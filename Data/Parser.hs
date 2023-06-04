{-# LANGUAGE Rank2Types #-}

module YAMP.Data.Parser (
   Parser, runParser, parseUsing, nextToken, peek, mapInput,
   module YAMP.Data.Result,
   module YAMP.Data.Stream,
   module Control.Applicative,
   module Control.Monad,
   module Control.Monad.Zip
) where

--------------------------------------------------------------------------------

import YAMP.Data.Stream
import YAMP.Data.Result

import Control.Applicative
import Control.Monad
import Control.Monad.Zip

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------

-- A Parser takes a stream of tokens and produces zero or more results
-- m = underlying monad, output will be wrapped in this (e.g. [] or Maybe)
-- t = tokens read (e.g. Char if input is a String)
-- a = output type (i.e. the data structure you're parsing)
data Parser m t a = Parser {
   run :: forall s. Stream s t => s -> m (Result s a)
}

type StdOut a = [Result String a]

runParser :: Stream s t => Parser m t a -> s -> m (Result s a)
runParser = run

parseUsing :: (Stream s t, MonadPlus m) => Parser m t a -> s -> m a
parseUsing p s = run p s >>= finalise

readerToParser :: ReadS a -> Parser [] Char a
readerToParser f = Parser $ \s -> do
   (x,r) <- f (toList s)
   pure $ toResult (x, fromList r)

--------------------------------------------------------------------------------

instance Functor m => Functor (Parser m t) where
   fmap f p = Parser $ \s -> fmap f <$> run p s

instance Monad m => Applicative (Parser m t) where
   pure x = Parser $ \s -> pure $ toResult (x,s)
   p <*> q = Parser $ \s -> do
      result <- run p s
      let f = value result
      let r = remainder result
      run (f <$> q) r

instance (Monad m, Alternative m) => Alternative (Parser m t) where
   empty = Parser $ \s -> empty
   p <|> q = Parser $ \s -> run p s <|> run q s

instance Monad m => Monad (Parser m t) where
   p >>= f = Parser $ \s -> do
      result <- run p s
      let x = value result
      let r = remainder result
      run (f x) r
   
instance MonadPlus m => MonadPlus (Parser m t)
   {- grants access to mfilter et al. -}

instance Monad m => MonadZip (Parser m t) where
   mzip = liftM2 (,)

instance MonadPlus m => Semigroup (Parser m t a) where
   (<>) = (<|>)

instance MonadPlus m => Monoid (Parser m t a) where
   mempty = empty

--------------------------------------------------------------------------------

-- Consume one token
nextToken :: Alternative m => Parser m t t
nextToken = Parser (fmap toResult . next)

-- Consume nothing, produce ()
unit :: Applicative m => Parser m t ()
unit = Parser (pure . curry toResult ())

-- Parses without consuming tokens
peek :: Functor m => Parser m t a -> Parser m t a
peek p = Parser $ \s -> setInput s <$> run p s

-- Transform input stream before consuming
mapInput :: Applicative m => (forall s. Stream s t => s -> s) -> Parser m t ()
mapInput f = Parser (pure . curry toResult () . f)

-- Only returns results with minimal remaining input
greedy :: (Foldable m, MonadPlus m) => Parser m t a -> Parser m t a
greedy p = Parser $ \s -> let
   results = run p s -- m (Result s a)
   minrem = fromMaybe 0 $ foldr (liftA2 min) Nothing $ fmap (Just . slength . remainder) results
   in mfilter ((== minrem) . slength . remainder) results

