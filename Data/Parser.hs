{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module YAMP.Data.Parser (
   Parser, nextToken,
   Result,
   module YAMP.Data.Stream,
   module Control.Applicative,
   module Control.Monad,
   module Control.Monad.Zip
) where

--------------------------------------------------------------------------------

import Prelude hiding ((.), id, null)

import YAMP.Data.Stream
import YAMP.Data.Result
import Data.Tuple

import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Data.Bifunctor
import Control.Category

--------------------------------------------------------------------------------

-- A Parser takes a stream of tokens and produces zero or more results
data Parser m s a = Parser {
   runParser :: s -> m (Result s a)
}

--------------------------------------------------------------------------------

instance Functor m => Functor (Parser m t) where
   fmap f p = Parser $ \s -> fmap f <$> runParser p s

instance Monad m => Applicative (Parser m t) where
   pure x = Parser $ \s -> pure $ toResult (x,s)
   p <*> q = Parser $ \s -> do
      result <- runParser p s
      let f = value result
      let r = remainder result
      runParser (f <$> q) r

instance (Monad m, Alternative m) => Alternative (Parser m t) where
   empty = Parser $ \s -> empty
   p <|> q = Parser $ \s -> runParser p s <|> runParser q s

instance Monad m => Monad (Parser m t) where
   p >>= f = Parser $ \s -> do
      result <- runParser p s
      let x = value result
      let r = remainder result
      runParser (f x) r
   
instance MonadPlus m => MonadPlus (Parser m t)
   {- grants access to mfilter et al. -}

instance Monad m => MonadZip (Parser m t) where
   mzip = liftM2 (,)

instance MonadPlus m => Semigroup (Parser m t a) where
   (<>) = (<|>)

instance MonadPlus m => Monoid (Parser m t a) where
   mempty = empty

--------------------------------------------------------------------------------

nextToken :: (Stream s t, Alternative m) => Parser m s t
nextToken = Parser $ \s -> fmap toResult $ next s
