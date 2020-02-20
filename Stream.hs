{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AbLib.Parser.Stream where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text'
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Char8 as BStrC
import qualified Data.ByteString.Lazy as BStr'
import qualified Data.ByteString.Lazy.Char8 as BStrC'

import GHC.Word (Word8)

class Monoid s => Stream c s where
   uncons :: s -> Maybe (c, s)
   
instance Stream a [a] where
   uncons = List.uncons
   
instance Stream Char Text.Text where
   uncons = Text.uncons
   
instance Stream Char Text'.Text where
   uncons = Text'.uncons
   
instance Stream Word8 BStr.ByteString where
   uncons = BStr.uncons
   
instance Stream Word8 BStr'.ByteString where
   uncons = BStr'.uncons

instance Stream Char BStrC.ByteString where
   uncons = BStrC.uncons

instance Stream Char BStrC'.ByteString where
   uncons = BStrC'.uncons