module Polar.Assets.Manager where

--import Polar.Types.AssetManager
import Control.Exception (IOException, try)

loadAsset :: FilePath -> IO (Either IOException String)
loadAsset name = try (readFile ("assets/" ++ name))

--getAsset :: Asset a => String -> AssetManager -> a
--getAsset name manager = do

requireAsset :: FilePath -> IO String
requireAsset name = loadAsset name >>= either l r
  where l _ = fail $ "failed to load asset '" ++ name ++ "'"
        r a = putStrLn ("loaded asset '" ++ name ++ "'") >> return a
