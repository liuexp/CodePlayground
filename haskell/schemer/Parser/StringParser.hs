module Parser.StringParser where
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.Primitives
doubleQuote = char '\"'
escapedChar = do char '\\'
		 x <- oneOf "\\\"tnr"
		 case x of 
		      't' -> return "\t"
		      'n' -> return "\n"
		      'r' -> return "\r"
		      '\\' -> return "\\"
		      '\"' -> return "\""

stringParser :: Parser Primitives
stringParser =
    do doubleQuote
       x <- many $ many1 (noneOf "\"\\") <|> escapedChar
       doubleQuote
       return $String $concat x



