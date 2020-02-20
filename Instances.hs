
module AbLib.Parser.Instances where

import AbLib.Parser.Data

import qualified Data.List as List

instance Parse String Char where
   parser = Parser $ maybeToList . List.uncons
   
-- instance Stream c s => Parse s [c] where
   -- parser = many next