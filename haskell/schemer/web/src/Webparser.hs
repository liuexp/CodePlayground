module Webparser
	(runOne) where
import Parser.Primitives
import Parser (exprParser)
import ScmShow
import Eval
import ScmError
import Env
import ReadExpr

import IO hiding (try)
import Control.Monad.Error
import Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List.Split
import Data.String.Utils


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExprList expr) >>= liftM tryLast . mapM (eval env)



runOne :: String -> IO String
runOne expr = primitiveBindings >>=liftM ("; Value: "++ ).flip evalString ("(load \"stdlib.scm\")\n" ++ expr)


