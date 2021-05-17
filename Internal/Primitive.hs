

module YAMP.Internal.Primitive where

import Prelude hiding (null)

import YAMP.Data.Parser
import YAMP.Data.CharStream

import Control.Monad
import Control.Arrow ((***))
import Data.Maybe (maybeToList)
import Data.List (iterate)

--------------------------------------------------------------------------------

char :: CharStream s => Parser s Char
char = Parser $ maybeToList . uncons

string :: CharStream s => Parser s s
string = many char >>= return . foldr cons empty

digit :: (CharStream s, Integral a) => Parser s a
digit = do
   n <- char
   guard ('0' <= n && n <= '9')
   return $ fromIntegral (fromEnum n - 48)

int :: (CharStream s, Integral a) => Parser s a
int = some digit >>= return . sum . zipWith (*) (iterate (*10) 1) . reverse

newline :: CharStream s => Parser s Char
newline = do
   c <- char
   guard (c == '\n' || c == '\r' || c == '\f')
   return c

eof :: CharStream s => Parser s ()
eof = Parser $ \s -> [((),s) | null s]