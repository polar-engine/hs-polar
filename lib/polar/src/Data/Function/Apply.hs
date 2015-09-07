module Data.Function.Apply (apply) where

apply :: (a -> b) -> a -> b
apply = ($)
infixr 9 `apply`
