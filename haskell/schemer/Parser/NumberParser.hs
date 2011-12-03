module Parser.NumberParser where
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Ratio
import Complex

import Parser.Primitives

numberParser = return =<<  digitParser <|> digitParser2 <|> hexParser <|> octParser <|> binParser <?> "valid number format"

negDigitParser = do try $ char '-'
		    return . Number . (0 -). read =<< many1 digit
digitParser = 
	return .Number . read =<< many1 digit
digitParser2 = do try $ string "#d"
		  return .Number . read =<< many1 digit
hexParser= do try $ string "#x"
              x <- many1 hexDigit
	      notFollowedBy alphaNum
              return $ Number (hex2dig x)

octParser = do try $ string "#o"
               x <- many1 octDigit
	       notFollowedBy alphaNum
               return $ Number (oct2dig x)

binParser = do try $ string "#b"
               x <- many1 (oneOf "10")
	       notFollowedBy alphaNum
               return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
--tail recursive
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = 
		let old = 2 * digint + (if x == '0' then 0 else 1) in
                        bin2dig' old xs


