{-# LANGUAGE MultiParamTypeClasses #-}

module YAMP.Data.Parse where

import YAMP.Data.Parser

--------------------------------------------------------------------------------

class Parse t a where
   parser :: Parser m t a
   -- parser = readerToParser reader
   
   reader :: ReadS a
   -- reader = runParser parser

   -- parse :: (Stream s t, MonadPlus m) => s -> m a
   -- parse s = parseUsing parser
   
   -- {-# MINIMAL parser | reader #-}

-- instance Parse Int where
   -- reader = readsPrec 0

-- data X=X deriving (Show)
-- instance Parse Char X where
   -- parser = do
      -- c <- nextToken
      -- guard (c=='X')
      -- pure X


-- qwe :: Stream s Char => Parser m s X
-- qwe = do
   -- c <- nextToken
   -- guard (c=='X')
   -- pure X