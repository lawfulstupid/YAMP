{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module YAMP.Data.Stream (
   Stream(..)
) where

import AbLib.Control.Monad (remonad)

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

   slength :: s -> Int
   slength = length . toList

   smap :: (t -> t) -> s -> s
   smap f = fromList . map f . toList

   {-# MINIMAL next, fromList #-}

instance Stream [a] a where
   next = remonad . uncons
   isEmpty = null
   toList = id
   fromList = id
   slength = length

instance Stream BS.ByteString Char where
   next = remonad . BS.uncons
   isEmpty = BS.null
   toList = BS.unpack
   fromList = BS.pack
   slength = BS.length

instance Stream BS'.ByteString Char where
   next = remonad . BS'.uncons
   isEmpty = BS'.null
   toList = BS'.unpack
   fromList = BS'.pack
   slength = fromIntegral . BS'.length

instance Stream T.Text Char where
   next = remonad . T.uncons
   isEmpty = T.null
   toList = T.unpack
   fromList = T.pack
   slength = T.length

instance Stream T'.Text Char where
   next = remonad . T'.uncons
   isEmpty = T'.null
   toList = T'.unpack
   fromList = T'.pack
   slength = fromIntegral . T'.length

