



   


{- Repeat a parsers exactly n times. -}
exactly :: Int -> Parser a -> Parser [a]
exactly n = between (n,n)

{- Repeat a parser between a and b times. -}
between :: (Int, Int) -> Parser a -> Parser [a]
between (a,b) f = do
   xs <- many f
   guard (a <= length xs && length xs <= b)
   return xs

{- Parses without consuming characters -}
peek :: Parser a -> Parser a
peek f = Parser $ \s -> [ (x,s) | (x,_) <- runParser f s ]
   
{- Applies a function to the input string -}
inputMap :: (s -> s) -> Parser s ()
inputMap f = Parser $ \s -> [ ((), f s) ]


{- Only returns results with minimal remaining input -}
greedy :: Parser a -> Parser a
greedy f = Parser $ \s -> let
   result = sortOn (length . snd) $ runParser f s
   maxlen = length $ snd $ head result
   in takeWhile ((maxlen ==) . length . snd) result


{- Matches a value exactly using its String representation. -}
match :: (Parse a, Eq a) => a -> Parser a
match t = do
   x <- parser
   guard (x == t)
   return x
   
matchString t = do
   s <- string
   guard (s == t)
   return s

matchSeq :: (Parse a, Eq a) => [a] -> Parser s [a]
matchSeq [] = return []
matchSeq t = do
   c <- char
   guard (c == head t)
   s <- matchSeq $ tail t
   return (c:s)

{- Match one of the given values -}
matchOne :: (Eq a, Parse a) => [a] -> Parser a
matchOne = mconcat . map match

{- Parses values pass a test. -}
{- Useful in combination with `Data.Char` functions like isSpace. -}
matchIf :: (Char -> Bool) -> Parser Char
matchIf f = do
   c <- next
   guard (f c)
   return c


{- Attempts to use a `Parser`, and switches to the second only if the first fails. -}
onFail :: Parser s a -> Parser s a -> Parser s a
onFail p q = Parser $ \s -> case runParser p s of
   [] -> runParser q s
   xs -> xs
   
onFail p q = Parser $ \s -> case runParser p s of {[] -> runParser q s; xs -> xs}

{- Tries the parsers in order, only uses later parsers if all previous ones fail -}
coalesce :: [Parser s a] -> Parser s a
coalesce = foldr onFail mempty

{- Parse a list of parseable items using given delimitor. -}
parseList :: (Eq s, Parse s) => (s, s, s) -> Parser a -> Parser [a]
parseList (left, delim, right) item = do
   match left                             -- left bracket required
   x <- return [] <|> do                  -- following is optional
      h <- item                              -- first item
      t <- many (match delim >> item)        -- 0+ trailing items
      return (h : t)                         -- assemble list
   match right                            -- right bracket required
   return x

maybeP :: Parser a -> Parser (Maybe a)
maybeP f = return Nothing <|> (Just <$> f)

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP f g = (Left <$> f) <|> (Right <$> g)



