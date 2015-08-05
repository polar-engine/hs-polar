{-# LANGUAGE LambdaCase #-}

module Polar.Listener where

import qualified Data.Map as M
import Control.Monad.State
import Polar.Types

listen :: Event -> Listener -> PolarIO ()
listen event listener = modify (mapListeners f)
  where f listeners = case M.lookup event listeners of
            Nothing -> M.insert event [listener] listeners
            Just xs -> M.insert event (xs ++ [listener]) listeners

notify :: Event -> Notification -> PolarIO ()
notify event note = do
    gets (M.lookup event . engineListeners) >>= \case
        Nothing -> return ()
        Just xs -> mapM_ (\x -> x note) xs
