{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module YAMP.Data.StreamState where

import YAMP.Data.Stream
import Data.Functor

data StreamState s x t = StreamState
   { stream :: s
   , state :: x }

class State x t where
   update :: x -> t -> x

instance (Stream s t, State x t) => Stream (StreamState s x t) t where
   next (StreamState s x) = next s <&> \(t,s') -> (t, StreamState s' $ update x t)
