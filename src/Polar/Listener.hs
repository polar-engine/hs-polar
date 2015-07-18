{-# LANGUAGE LambdaCase #-}

module Polar.Listener where

import qualified Data.Map as M
import Control.Monad.State
import Polar.Types

listen :: Notification -> Listener -> PolarIO ()
listen note listener = modify (mapListeners f)
  where f listeners = case M.lookup note listeners of
            Nothing -> M.insert note [listener] listeners
            Just xs -> M.insert note (xs ++ [listener]) listeners

notify :: Notification -> PolarIO ()
notify note = do
    gets (M.lookup note . engineListeners) >>= \case
        Nothing -> return ()
        Just xs -> mapM_ (\x -> x note) xs
