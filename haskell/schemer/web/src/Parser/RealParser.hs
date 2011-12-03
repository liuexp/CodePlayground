module Parser.RealParser where
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Ratio
import Complex

import Parser.Primitives
import Parser.NumberParser

negRealParser = do try $ char '-'
		   x <- many1 digit
                   char '.'
                   y <- many1 digit
                   return $ Real.(0 -)$ (fst.head$readFloat (x++"."++y))


realParser :: Parser Primitives
realParser = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Real (fst.head$readFloat (x++"."++y))

negRatioParser = do try $ char '-'
		    x <- many1 digit
		    char '/'
		    y <- many1 digit
		    return $ Ratio.(0 -)$ ((read x) % (read y))

ratioParser :: Parser Primitives
ratioParser = do x <- many1 digit
                 char '/'
                 y <- many1 digit
                 return $ Ratio ((read x) % (read y))

negComplexParser :: Parser Primitives
negComplexParser = do try $ char '-'
		      x <- (try realParser <|> numberParser)
		      z <- try (char '+') <|> char '-'
		      y <- (try realParser <|> try numberParser <|> try negDigitParser <|> negRealParser)
		      char 'i' -- <|> char 'j' 
		      return $ Complex ((0.0 -)( toReal x)  :+ case z of
							       '+' -> toReal y
							       '-' -> (0 -). toReal $y
							       )


complexParser :: Parser Primitives
complexParser = do x <- (try realParser <|> numberParser)
		   z <- try (char '+') <|> char '-'
                   y <- (try realParser <|> numberParser)
                   char 'i' 
		   return $ Complex ( toReal x  :+ case z of
							       '+' -> toReal y
							       '-' -> (0 -). toReal $y
							       )



