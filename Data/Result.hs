module YAMP.Data.Result where

import YAMP.Data.Stream
import Control.Monad

--------------------------------------------------------------------------------

data Result s a = Result s a

--------------------------------------------------------------------------------

instance (Show a, Show s) => Show (Result s a) where
   show (Result s a) = "(" ++ show a ++ "|" ++ show s ++ ")"

instance Functor (Result s) where
   fmap f (Result s x) = Result s $ f x

--------------------------------------------------------------------------------

toResult :: (a,s) -> Result s a
toResult (x,s) = Result s x

fromResult :: Result s a -> (a,s)
fromResult (Result s x) = (x,s)

value :: Result s a -> a
value (Result _ a) = a

remainder :: Result s a -> s
remainder (Result s _) = s

setInput :: s -> Result s a -> Result s a
setInput s (Result _ x) = Result s x

-- checks if input stream is empty
complete :: Stream s t => Result s a -> Bool
complete = isEmpty . remainder

-- converts completed results (input stream is empty) into parser output
finalise :: (Stream s t, MonadPlus m) => Result s a -> m a
finalise result = do
   guard $ complete result
   pure $ value result
