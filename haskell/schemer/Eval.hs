module Eval where
import Parser.Primitives
import ScmError
import ScmShow
import Env
import ReadExpr
import Control.Monad.Error
import Data.IORef
import IO
import Parser (exprParser)
import Text.ParserCombinators.Parsec hiding (spaces)
--import Complex
--import Ratio
-- TODO : display & trace
eval :: Env -> Primitives -> IOThrowsError Primitives
eval env val@(Comment _) = return $Comment ";<comments>"
eval env val@(Verbatim _) = return val
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Real _) = return val
eval env val@(Ratio _) = return  val
eval env val@(Complex _) = return  val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         Bool True  -> eval env conseq
	 _	    -> throwError $ TypeMismatch "boolean" pred

eval env (List (Atom "cond" : expr : rest)) = do
    eval' expr rest
    where eval' (List [cond, value]) (x : xs) = do
              result <- eval env cond
              case result of
                   Bool False -> eval' x xs
                   Bool True  -> eval env value
                   _	      -> throwError $ TypeMismatch "boolean" cond
          eval' (List [Atom "else", value]) [] = do
               eval env value
          eval' (List [cond, value]) [] = do
              result <- eval env cond
              case result of
                   Bool True  -> eval env value
                   _	      -> throwError $ TypeMismatch "boolean" cond
	  eval' param _ = throwError $ TypeMismatch "conditional" param

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
-- FIXED: (define a (define b 1) (+ b 1) ) won't pollute global environment
eval env (List (Atom "define": Atom var: form)) = do
    liftIO (bindVars env []) >>=evalBody  >>= defineVar env var
    where evalBody tmpEnv = liftM tryLast $ mapM (eval tmpEnv)  form
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalProcedure env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalProcedure env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM tryLast . mapM (eval env)
--eval env (List [Atom "display", String str]) = return $ Verbatim $ str
eval env (List [Atom "display", String str]) =do
	liftIO $ putStrLn $ str 
	return $ Comment $ str
eval env (List [Atom "display", arg]) = do
	liftIO . putStrLn . show =<< eval env arg
	return $ Comment $ ""
eval env val@(List (Atom "define" : args)) =  throwError $ BadSpecialForm "Unrecognized special form of " val

eval env (List (Atom "force" : args)) = 
    eval env (List args)
eval env (List (Atom "delay" : args)) = 
    eval env (List (Atom "lambda" : List [] : args))

-- Warning: it's better to treat + as bindings to environment so that (define - +) won't fail
{-eval env (List (Atom "+" : args)) = do 
    argVals <- mapM (eval env) args
    func <- tryLookup env "+"  $"add"++getType argVals
    apply func argVals
-}
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeProcedure varargs env params body = return $ Procedure (map showVal params) varargs body env
makeNormalProcedure = makeProcedure Nothing
makeVarargs = makeProcedure . Just . showVal

getType x
	| hasComplex x = "Complex"
	| hasReal x = "Real"
	| hasRatio x = "Ratio"
	| otherwise = "Number"

tryLast []=String "Welcome!"
tryLast a = last a

tryLookup :: Env -> String -> String -> IOThrowsError Primitives
tryLookup envRef x y = do env <- liftIO $ readIORef envRef
			  maybe (eval envRef $Atom y)
                               (liftIO . readIORef)
                               (lookup x env)

apply :: Primitives -> [Primitives] -> IOThrowsError Primitives
apply (PrimitiveProc func) args = liftThrows $ func args
apply (Procedure params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 
apply (IOFunc func) args = func args
apply badForm _ = throwError $ BadSpecialForm "Unrecognized special form" badForm

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveProc) anyPrimitives)
    where makeFunc constructor (var, func) = (var, constructor func)


ioPrimitives :: [(String, [Primitives] -> IOThrowsError Primitives)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [Primitives] -> IOThrowsError Primitives
applyProc [] = throwError $ NumArgs 1 []
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [Primitives] -> IOThrowsError Primitives
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [Primitives] -> IOThrowsError Primitives
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False



readProc :: [Primitives] -> IOThrowsError Primitives
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [Primitives] -> IOThrowsError Primitives
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [Primitives] -> IOThrowsError Primitives
readContents [String filename] = liftM String $ liftIO $ readFile filename


load :: String -> IOThrowsError [Primitives]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [Primitives] -> IOThrowsError Primitives
readAll [String filename] = liftM List $ load filename



liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue



