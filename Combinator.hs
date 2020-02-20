
module AbLib.Parser.Combinator where

char :: Parse s Char => Parser s Char
char = parser

match :: (Parse s t, Eq t) => t -> Parser s t
match t = parser >>= guard . (t ==) >> return t