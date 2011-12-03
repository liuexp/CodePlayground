module Env where
import Parser.Primitives
import ScmError
import ReadExpr
import Control.Monad.Error
import Data.IORef
import IO
import Complex
import Ratio

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError Primitives
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "; Unbound variable " var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> Primitives -> IOThrowsError Primitives
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "; Unbound variable " var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> Primitives -> IOThrowsError Primitives
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, Primitives)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)


----------Primitives bindings--------
anyPrimitives :: [(String, [Primitives] -> ThrowsError Primitives)]
anyPrimitives = [("+" , addOp) ,
		("-" , subOp) ,
		("*" , mulOp) ,
		("/" , divOp) ,
		{-("addInteger" , numericBinop (+)) ,
              ("subInteger" , numericBinop (-)) ,
              ("mulInteger" , numericBinop (*)) ,
              ("divInteger" , numericBinop div) ,
	      ("addRatio" , ratioBinop (+)) ,
              ("subRatio" , ratioBinop (-)) ,
              ("mulRatio" , ratioBinop (*)) ,
              ("divRatio" , ratioBinop (/)) ,
		("addReal" , realBinop (+)) ,
              ("subReal" , realBinop (-)) ,
              ("mulReal" , realBinop (*)) ,
              ("divReal" , realBinop (/)) ,
		("addComplex" , complexBinop (+)) ,
              ("subComplex" , complexBinop (-)) ,
              ("mulComplex" , complexBinop (*)) ,
              ("divComplex" , complexBinop (/)) ,
	      -}
              ("mod" , numericBinop mod) ,
              ("gcd" , numericBinop gcd) ,
              ("lcm" , numericBinop lcm) ,
              ("quotient" , numericBinop quot) ,
              ("remainder" , numericBinop rem) ,
              ("symbol?" , unaryOp symbolp) ,
              ("string?" , unaryOp stringp) ,
              ("number?" , unaryOp numberp) ,
              ("bool?", unaryOp boolp) ,
              ("list?" , unaryOp listp),
	      ("=", numEq ),
              ("<", numLt), 
              (">", numGt ),
              ("/=", numNeq ),
              (">=", numGeq ),
              ("<=", numLeq ),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
	      ("and", boolBoolBinop (&&)),
              ("or", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
	      ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
	]




symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

subOp :: [Primitives] -> ThrowsError Primitives
subOp [(Complex x)] = return $ Complex $ (- x)
subOp [(Real x)] = return $ Real $ (- x)
subOp [(Ratio x)] = return $ Ratio $ (- x)
subOp [(Number x)] = return $ Number $ (- x)

subOp params 
	| null params =  throwError $ NumArgs 2 params
	| hasComplex params = return . Complex . foldl1 (-)  =<< mapM unpackComplex params 
	| hasReal params = return . Real . foldl1 (-)  =<< mapM unpackReal params 
	| hasRatio params = return . Ratio . foldl1 (-)  =<< mapM unpackRatio params 
	| otherwise = return . Number . foldl1 (-)  =<< mapM unpackNum params 

addOp params 
	| null params =  throwError $ NumArgs 2 params
	| hasComplex params = return . Complex . foldl1 (+)  =<< mapM unpackComplex params 
	| hasReal params = return . Real . foldl1 (+)  =<< mapM unpackReal params 
	| hasRatio params = return . Ratio . foldl1 (+)  =<< mapM unpackRatio params 
	| otherwise = return . Number . foldl1 (+)  =<< mapM unpackNum params 

mulOp params 
	| null params =  throwError $ NumArgs 2 params
	| hasComplex params = return . Complex . foldl1 (*)  =<< mapM unpackComplex params 
	| hasReal params = return . Real . foldl1 (*)  =<< mapM unpackReal params 
	| hasRatio params = return . Ratio . foldl1 (*)  =<< mapM unpackRatio params 
	| otherwise = return . Number . foldl1 (*)  =<< mapM unpackNum params 

divOp [(Complex 0)] =  throwError $ Default "; Runtime Exception, division by 0 "
divOp [(Complex x)] = return $ Complex $ (1/x)
divOp [(Real 0)] =  throwError $ Default "; Runtime Exception, division by 0 "
divOp [(Real x)] = return $ Real $ (1/ x)
divOp [(Ratio 0)] =  throwError $ Default "; Runtime Exception, division by 0 "
divOp [(Ratio x)] = return $ Ratio $ (1/ x)
divOp [(Number 0)] =  throwError $ Default "; Runtime Exception, division by 0 "
divOp [(Number x)] = return $ Number $ (1 `div` x)

divOp params 
	| null params =  throwError $ NumArgs 2 params
	| hasZero $tail params = throwError $ Default "; Runtime Exception, division by 0 "
	| hasComplex params = return . Complex . foldl1 (/)  =<< mapM unpackComplex params 
	| hasReal params = return . Real . foldl1 (/)  =<< mapM unpackReal params 
	| otherwise = return . Ratio . foldl1 (/)  =<< mapM unpackRatio params 


{-
complexBinop (-) params@[_] = return . Complex . foldl (-) 0 =<< mapM unpackComplex params
complexBinop op params = return . Complex . foldl1 op =<< mapM unpackComplex params 
complexBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal


realBinop (-) params@[_] = return . Real . foldl (-) 0 =<< mapM unpackReal params
realBinop op params = return . Real . foldl1 op =<< mapM unpackReal params 
realBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal


ratioBinop (-) params@[_] = return . Ratio . foldl (-) 0 =<< mapM unpackRatio params
ratioBinop op params = return . Ratio . foldl1 op =<< mapM unpackRatio params 
ratioBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal


-}
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = return . Number . foldl1 op =<< mapM unpackNum params 

unaryOp f [v] = return $f v
boolBinop :: (Primitives -> ThrowsError a) -> (a -> a -> Bool) -> [Primitives] -> ThrowsError Primitives
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

boolOp :: (Primitives -> ThrowsError a) -> (a -> a -> Bool) -> [Primitives] -> ThrowsError Primitives
boolOp unpacker op args = if length args < 1 
                          then throwError $ NumArgs 2 args
			  --else return . Bool . foldl (&&) True . map (\p -> (fst p ) `op` (snd p)).(\x -> zip x (tail x)) =<< mapM unpacker args
			  else return . Bool . null . dropWhile (== True) . map (\p -> (fst p ) `op` (snd p)).(\x -> zip x (tail x)) =<< mapM unpacker args

--numBoolBinop op args = boolOp unpackNum op args
numEq args
	| hasComplex args = boolOp unpackComplex (==) args
	| hasReal args = boolOp unpackReal (==) args
	| hasRatio args = boolOp unpackRatio (==) args
	| otherwise = boolOp unpackNum (==) args

numLt args
	| hasComplex args = throwError $ TypeMismatch "orderable number" $List args
	| hasReal args = boolOp unpackReal (<) args
	| hasRatio args = boolOp unpackRatio (<) args
	| otherwise = boolOp unpackNum (<) args
numGt args
	| hasComplex args = throwError $ TypeMismatch "orderable number" $List args
	| hasReal args = boolOp unpackReal (>) args
	| hasRatio args = boolOp unpackRatio (>) args
	| otherwise = boolOp unpackNum (>) args
numGeq args
	| hasComplex args = throwError $ TypeMismatch "orderable number" $List args
	| hasReal args = boolOp unpackReal (>=) args
	| hasRatio args = boolOp unpackRatio (>=) args
	| otherwise = boolOp unpackNum (>=) args
numLeq args
	| hasComplex args = throwError $ TypeMismatch "orderable number" $List args
	| hasReal args = boolOp unpackReal (<=) args
	| hasRatio args = boolOp unpackRatio (<=) args
	| otherwise = boolOp unpackNum (<=) args
numNeq args
	| hasComplex args = boolOp unpackComplex (/=) args
	| hasReal args = boolOp unpackReal (/=) args
	| hasRatio args = boolOp unpackRatio (/=) args
	| otherwise = boolOp unpackNum (/=) args
strBoolBinop = boolOp unpackStr
boolBoolBinop = boolOp unpackBool



unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
unpackReal (Real n) = return n
unpackReal (Number n) = return $ toReal $Number n
unpackReal (Ratio n) = return $ toReal $ Ratio n
unpackReal (String n) =  let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "complex number" $ String n
                            else return $ fst $ parsed !! 0
unpackReal (List [n]) = unpackReal n

unpackComplex  :: Primitives -> ThrowsError (Complex Double)
unpackComplex (Complex n) = return n
unpackComplex (Number n) = unpackComplex $ Real $toReal $ Number n 
unpackComplex (Real n) = return $ n:+ 0
unpackComplex (Ratio n) = unpackComplex $ Real $ toReal $ Ratio n
unpackComplex (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "complex number" $ String n
                            else return $ fst $ parsed !! 0
unpackComplex (List [n]) = unpackComplex n


--unpackRatio  :: Primitives -> ThrowsError Ratio
unpackRatio (Ratio n) = return n
unpackRatio (Number n) = unpackRatio $Ratio $ n%1
unpackRatio (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "ratio number" $ String n
                            else return $ fst $ parsed !! 0
unpackRatio (List [n]) = unpackRatio n


unpackStr :: Primitives -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: Primitives -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

eqv :: [Primitives] -> ThrowsError Primitives
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
			       Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (Primitives -> ThrowsError a)
unpackEquals :: Primitives -> Primitives -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)
equal :: [Primitives] -> ThrowsError Primitives
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


----List cons car cdr
car :: [Primitives] -> ThrowsError Primitives
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [Primitives] -> ThrowsError Primitives
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Primitives] -> ThrowsError Primitives
cons [x1, List []] = return $ List [x1]

cons [x, List xs] = return $ List $ x : xs

cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast

cons [x1, x2] = return $ DottedList [x1] x2

cons badArgList = throwError $ NumArgs 2 badArgList


symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""


