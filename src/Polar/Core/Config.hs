{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Module      : Polar.Core.Config
  Copyright   : (c) 2015-2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Engine configuration functions.
-}

module Polar.Core.Config (startupConfig, getConfig) where

import Control.Lens.Setter (assign)
import Polar.ConfigFile
import Polar.Types
import {-# SOURCE #-} Polar.Log (logFatal)
import Polar.Core.File

startupConfig :: Core ()
startupConfig = use config >>= load >>= assign config
  where load cp = loadFileNow "engine.cfg" >>= forceParse . readFromString cp
        forceParse (Left (err, _)) = logFatal ("failed to load config file (" ++ show err ++ ")")
        forceParse (Right cp) = pure cp

class GetConfig a where getConfig :: ConfigProxy a -> SectionName -> OptionName -> Core a
instance Read a => GetConfig a where getConfig _ s o = (\c -> get c s o) <$> use config >>= forceOption

forceOption :: Either ConfigError a -> Core a
forceOption (Left (err, _)) = logFatal ("failed to get config option (" ++ show err ++ ")")
forceOption (Right x) = pure x
