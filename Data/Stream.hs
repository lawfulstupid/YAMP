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
   
   isEmpty :: s -> Bool
   isEmpty = isNothing . next
   
   toList :: Stream s t => s -> [t]
   toList s = next s >>= uncurry (:) . fmap toList
   
   fromList :: Stream s t => [t] -> s
   
   {-# MINIMAL next, fromList #-}

maybeToMonad :: Alternative m => Maybe a -> m a
maybeToMonad = \case
   Nothing -> empty
   Just x  -> pure x

instance Stream [a] a where
   next = maybeToMonad . uncons
   isEmpty = null
   toList = id
   fromList = id

instance Stream BS.ByteString Char where
   next = maybeToMonad . BS.uncons
   isEmpty = BS.null
   toList = BS.unpack
   fromList = BS.pack

instance Stream BS'.ByteString Char where
   next = maybeToMonad . BS'.uncons
   isEmpty = BS'.null
   toList = BS'.unpack
   fromList = BS'.pack

instance Stream T.Text Char where
   next = maybeToMonad . T.uncons
   isEmpty = T.null
   toList = T.unpack
   fromList = T.pack

instance Stream T'.Text Char where
   next = maybeToMonad . T'.uncons
   isEmpty = T'.null
   toList = T'.unpack
   fromList = T'.pack
