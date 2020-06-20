module YAMP.Data.Result (
   Result, toResult, value, remainder
) where

import YAMP.Data.Stream

--------------------------------------------------------------------------------

data Result s a = Result s a

--------------------------------------------------------------------------------

instance Functor (Result s) where
   fmap f (Result s x) = Result s $ f x

--------------------------------------------------------------------------------

toResult :: (a,s) -> Result s a
toResult (x,s) = Result s x

value :: Result s a -> a
value (Result _ a) = a

remainder :: Result s a -> s
remainder (Result s _) = s

complete :: Stream s t => Result s a -> Bool
complete = isEmpty . remainder
