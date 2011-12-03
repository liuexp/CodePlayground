module Parser.Primitives where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Ratio
import Complex
import Control.Monad.Error
import Data.IORef
import IO hiding (try)

spaces :: Parser ()
spaces = skipMany1 $ space-- <|> char '\n'

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

type Env = IORef [(String, IORef Primitives)]

type ThrowsError = Either ScmError

type IOThrowsError = ErrorT ScmError IO

data ScmError = NumArgs Integer [Primitives]
               | TypeMismatch String Primitives
               | Parser ParseError
               | BadSpecialForm String Primitives
               | NotFunction String String
               | UnboundVar String String
               | Default String

data Primitives = Atom String
	| List [Primitives]
	| DottedList [Primitives] Primitives
	| Number Integer
	| Real Double
	| Ratio Rational
	| Complex (Complex Double)
	| String String
	| Verbatim String
	| Bool Bool
	| PrimitiveProc ([Primitives] -> ThrowsError Primitives)
        | Procedure {params :: [String], vararg :: (Maybe String), 
                      body :: [Primitives], closure :: Env}
        | IOFunc ([Primitives] -> IOThrowsError Primitives)
        | Port Handle
	| Comment String

{-data ScmNum = IntNumber Integer
	| Real Double
	| Ratio Rational
	| Complex (Complex Double)
-}

--isZero =  (== 0) =<< unpackNum
isZero (Number n) = (==) n 0
isZero (Complex n) = (==) n 0
isZero (Real n) = (==) n 0
isZero (Ratio n) = (==) n 0
isZero _ = False
hasZero = foldl (||) False .map isZero

isRatio (Ratio n) = True
isRatio _ = False
hasRatio = not. null. dropWhile (==False). map isRatio

isComplex (Complex n) = True
isComplex _ = False
hasComplex = not. null. dropWhile (==False). map isComplex

isReal (Real n) = True
isReal _ = False
hasReal = not. null. dropWhile (==False). map isReal

toReal :: Primitives -> Double
toReal (Real r) = r
toReal (Number n) = fromIntegral n
toReal (Ratio n) = fromRational n


isComment (Comment s) = True
isComment _ = False
