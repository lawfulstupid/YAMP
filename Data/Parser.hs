{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module YAMP.Data.Parser (
   Parser,
   module YAMP.Data.Stream,
   module Control.Applicative,
   module Control.Monad,
   module Control.Monad.Zip
) where

import Prelude hiding ((.), id, null)

import YAMP.Data.Stream
import YAMP.Data.ParseResult
import Data.Tuple

import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Data.Bifunctor
import Control.Category

--------------------------------------------------------------------------------

-- A Parser takes a stream (s) and produces one or more (m) results (a)
data Parser m s a = Parser {
   runParser :: s -> m (ParseResult s a)
}

token :: (Stream s t, Alternative m) => Parser m s t
token = Parser (fmap result . next)

--------------------------------------------------------------------------------

instance Functor m => Functor (Parser m t) where
   fmap f p = Parser $ \s -> fmap f <$> runParser p s

instance Monad m => Applicative (Parser m t) where
   pure x = Parser $ \s -> pure (Result x s)
   p <*> q = Parser $ \s -> do
      Result f r <- runParser p s
      runParser (f <$> q) r

instance (Monad m, Alternative m) => Alternative (Parser m t) where
   empty = Parser $ \s -> empty
   p <|> q = Parser $ \s -> runParser p s <|> runParser q s

instance Monad m => Monad (Parser m t) where
   p >>= f = Parser $ \s -> do
      Result x r <- runParser p s
      runParser (f x) r
   
instance MonadPlus m => MonadPlus (Parser m t)
   {- grants access to mfilter et al. -}

instance Monad m => MonadZip (Parser m t) where
   mzip = liftM2 (,)

instance MonadPlus m => Semigroup (Parser m t a) where
   (<>) = (<|>)

instance MonadPlus m => Monoid (Parser m t a) where
   mempty = empty
