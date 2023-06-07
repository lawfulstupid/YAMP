module YAMP.Control.Parsers where

import YAMP.Data.Parser
import YAMP.Data.Types
import YAMP.Data.Stream (isEmpty, fromList)
import YAMP.Control.Combinators
import Data.Char (isSpace)

-- Matches a nonempty region of contiguous whitespace
whitespace :: MonadPlus m => Parser m Char String
whitespace = some $ charThat isSpace

-- Matches a nonempty region of contiguous horizontal whitespace
hspace :: MonadPlus m => Parser m Char String
hspace = some $ matchAny "\9\32\160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288"

-- Matches a nonempty region of contiguous vertical whitespace (newlines)
vspace :: MonadPlus m => Parser m Char String
vspace = some $ matchAny "\10\11\12\13\133\8232\8233"

-- Matches a letter
letter :: MonadPlus m => Parser m Char Char
letter = letter_upper <|> letter_lower

-- Matches an uppercase letter
letter_upper :: MonadPlus m => Parser m Char Char
letter_upper = matchAny ['A'..'Z']

-- Matches an lowercase letter
letter_lower :: MonadPlus m => Parser m Char Char
letter_lower = matchAny ['a'..'z']

-- Matches a digit
digit :: MonadPlus m => Parser m Char Char
digit = matchAny ['0'..'9']

-- Matches an alphanumeric character
alphanum :: MonadPlus m => Parser m Char Char
alphanum = letter <|> digit

-- Matches a normal identifier format
ident :: MonadPlus m => Parser m Char String
ident = liftA2 (:) letter $ many (alphanum <|> match '_')

-- Matches end of input
eof :: MonadPlus m => Parser m Char ()
eof = do
   mapInput (\s -> if isEmpty s then fromList "\3" else s)
   char '\3'
   pure ()
