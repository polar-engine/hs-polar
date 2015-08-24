module Polar.Shader where

import Data.List (nub)
import qualified Data.Map as M
import Control.Monad.RWS (runRWST)
import Polar.Shader.Types
import Polar.Shader.Tokenizer (tokenize)
import Polar.Shader.Parser (parse)
import qualified Polar.Shader.Processor as Processor

compile :: Compiler a => String -> M.Map String Int -> M.Map String Int -> a -> Either String (String, String)
compile contents ins outs compiler = do
    fns <- tokenize contents >>= parse
    (fns', _, (ins', outs')) <- runRWST Processor.process (CompilerEnv fns ins outs) undefined
    generate (CompilerEnv fns' (M.fromList (nub ins')) (M.fromList (nub outs'))) compiler
