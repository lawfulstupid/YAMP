module YAMP.Data.Parser (
   Parser, runParser, parseUsing, nextToken,
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

--------------------------------------------------------------------------------

-- A Parser takes a stream of tokens and produces zero or more results
data Parser m s a = Parser {
   run :: s -> m (Result s a)
}

runParser = run

parseUsing :: (Stream s t, MonadPlus m) => Parser m s a -> s -> m a
parseUsing p s = do
   result <- run p s
   guard $ complete result
   pure $ value result

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

nextToken :: (Stream s t, Alternative m) => Parser m s t
nextToken = Parser $ \s -> fmap toResult $ next s
