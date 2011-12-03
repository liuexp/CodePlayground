-- Warning: This file is deprecated.
module Parser.List where
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.Primitives
import Parser.Expr (exprParser)

listParser :: Parser Primitives
listParser = liftM List $ sepBy exprParser spaces

dottedListParser :: Parser Primitives
dottedListParser = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

quotedParser :: Parser Primitives
quotedParser = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
