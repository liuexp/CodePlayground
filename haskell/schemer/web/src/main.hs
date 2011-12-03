module Main where
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


flushStr str = putStr str >> hFlush stdout
--readPrompt prompt = flushStr prompt >> getLine
readPrompt expr = do
	flushStr msg
	x <- getLine
	if expr /= "" then return $ expr ++ " "++ x
		      else return x
	where msg
	      	| null expr = "SJTU-Scheme>>> "
	      	| otherwise = "... " ++ ( show $ countOpenParens expr) ++ " more >>>"

countOpenParens = foldl (\y x -> case x of
				 '(' -> (y+1)
				 ')' -> (y-1)
				 otherwise -> (y) ) 0


--printIOParse file = load file >>= putStrLn
printParse expr = primitiveBindings >>= flip printString expr >>= putStrLn

printString env expr = runIOThrows $ liftM show$(liftThrows $ readExprList expr )

evalAndPrint env expr =  evalString env expr >>= putStrLn. ("; Value: " ++ )
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExprList expr ) >>= liftM tryLast . mapM (eval env)

doUntil_ pred pred2 prompt init action = action init >> until_ pred pred2 prompt "" action

until_ pred pred2 prompt expr action = do 
  result <- if pred2 expr
     		then prompt ""
     		else prompt expr
  if pred result 
     then return ()
     else if pred2 result
     		then action result >> until_ pred pred2 prompt "" action
		else until_ pred pred2 prompt result action

repl  =	do 
	putStrLn "Welcome to SJTU-Scheme!  Type: (quit) to quit."
	primitiveBindings >>= doUntil_  ( == "(quit)") ((<=0).countOpenParens) (readPrompt) "(load \"stdlib.scm\")" . evalAndPrint 
	putStrLn "Bye!"



runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
         >>= hPutStrLn stderr
--main = print.eval.readExpr. (!! 0) =<< getArgs
main :: IO ()
main = do args <- getArgs
          if null args then repl
		  else runOne $ args
