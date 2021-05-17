
module AbLib.Parser.Combinator where

match :: (Parse s t, Eq t) => t -> Parser s t
match t = parser >>= guard . (t ==) >> return t



char :: Char -> Parser s Char
char c = nextToken >>= \t -> do
   guard (t == c)
   return c

string :: String -> Parser s String
string [] = pure []
string (c:s) = do
   t <- char c
   ts <- string s
   return (t:ts)

-- e.g. any char
any :: (t -> Parser s t) -> Parser s t

something :: Parser s a

peek :: Parser s a -> Parser s a
peek f = do
   