{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module YAMP.Data.Stream where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text'
import qualified Data.ByteString.Char8 as BStr
import qualified Data.ByteString.Lazy.Char8 as BStr'

--------------------------------------------------------------------------------

class Monoid s => Stream c s | s -> c where
   uncons :: s -> Maybe (c, s)

--------------------------------------------------------------------------------
   
instance Stream a [a] where
   uncons = List.uncons
   
instance Stream Char Text.Text where
   uncons = Text.uncons
   
instance Stream Char Text'.Text where
   uncons = Text'.uncons
   
instance Stream Char BStr.ByteString where
   uncons = BStr.uncons
   
instance Stream Char BStr'.ByteString where
   uncons = BStr'.uncons
