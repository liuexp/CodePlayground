module Parser.AtomParser where
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.Primitives
atomParser = 
	do x <- letter <|> symbol
	   y <- many $ letter <|> symbol <|> digit 
	   let ret = x:y
	   return $ case ret of 
			"#t" -> Bool True
			"#f" -> Bool False
			otherwise -> Atom ret
	
