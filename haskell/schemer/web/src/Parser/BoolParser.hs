module Parser.BoolParser where   
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.Primitives
boolParser :: Parser Primitives
boolParser = do char '#'
		x <- oneOf "tf"
                return $ case x of 
                          't' -> Bool True
                          'f' -> Bool False


