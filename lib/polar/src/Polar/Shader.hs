module Polar.Shader where

import Data.List (nub)
import qualified Data.Map as M
import Control.Monad.RWS (runRWST)
import Polar.Shader.Types
import Polar.Shader.Tokenizer (tokenize)
import Polar.Shader.Parser (parse)
import qualified Polar.Shader.Processor as Processor

compile :: Compiler a => String -> M.Map String DataType -> M.Map String DataType -> M.Map String DataType -> a -> Either String (String, String)
compile contents globals ins outs compiler = do
    fns <- tokenize contents >>= parse
    (fns', _, (globals', ins', outs')) <- runRWST Processor.process (CompilerEnv fns globals ins outs) undefined
    generate (CompilerEnv fns' (M.fromList (nub globals')) (M.fromList (nub ins')) (M.fromList (nub outs'))) compiler
