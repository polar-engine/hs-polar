module Polar.Hoist where

import Control.Monad.Trans.State

hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState
