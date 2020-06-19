module YAMP.Data.Result (
   Result, toResult, value, remainder
) where

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