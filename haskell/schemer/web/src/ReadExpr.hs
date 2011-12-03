module ReadExpr where
import Parser.Primitives
import Parser (exprParser)
import ScmError
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.String.Utils

readExpr :: String -> ThrowsError Primitives
readExpr = readOrThrow (skipMany space >> exprParser)
--readExprList = readOrThrow (endBy exprParser spaces)
readExprList :: String -> ThrowsError [Primitives]
readExprList expr= (liftM (filter (not . isComment))) $ readOrThrow (sepEndBy (skipMany space >> exprParser) spaces) (replace "\n" " \n " expr)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "(unknown)" input of
    Left err -> throwError $ Parser err
    Right val -> return val

