module Polar.Types.Engine
( Engine
) where

data Engine = Engine
    { value :: Int
    } deriving (Monad, MonadIO)

setValue :: Int -> Engine
setValue = do
