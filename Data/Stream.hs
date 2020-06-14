{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module YAMP.Data.Stream (
   Stream(..)
) where

import Control.Applicative
import Data.Maybe (isNothing)

import Data.List (uncons)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS'
import qualified Data.Text as T
import qualified Data.Text.Lazy as T'

--------------------------------------------------------------------------------

class Stream s t | s -> t where
   next :: Alternative m => s -> m (t,s)
   null :: s -> Bool
   null = isNothing . next
   {-# MINIMAL next #-}

maybeToMonad :: Alternative m => Maybe a -> m a
maybeToMonad = \case
   Nothing -> empty
   Just x  -> pure x

instance Stream [a] a where
   next = maybeToMonad . uncons

instance Stream BS.ByteString Char where
   next = maybeToMonad . BS.uncons

instance Stream BS'.ByteString Char where
   next = maybeToMonad . BS'.uncons

instance Stream T.Text Char where
   next = maybeToMonad . T.uncons

instance Stream T'.Text Char where
   next = maybeToMonad . T'.uncons
