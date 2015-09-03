module Control.Monad.Truthful where

import Data.Truthful

whenTruthful :: (Monad m, Truthful a, Monoid b) => m b -> a -> m b
whenTruthful action truthful
    | isTruthful truthful = action
    | otherwise = return mempty

unlessTruthful :: (Monad m, Truthful a, Monoid b) => m b -> a -> m b
unlessTruthful action truthful
    | notTruthful truthful = action
    | otherwise = return mempty

whenTruthful1 :: (Monad m, Truthful a, Monoid b) => (a -> m b) -> a -> m b
whenTruthful1 f truthful
    | isTruthful truthful = f truthful
    | otherwise = return mempty
