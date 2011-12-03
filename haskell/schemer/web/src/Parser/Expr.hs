module Parser.Expr (
exprParser
) where 
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Ratio
import Complex
import Parser.StringParser
import Parser.AtomParser
import Parser.BoolParser
import Parser.NumberParser
import Parser.RealParser
import Parser.Primitives
--import Parser.List

listParser :: Parser Primitives
listParser = do
	char '('
	try (skipMany space)
	x <- sepEndBy exprParser spaces -- <|> try (skipMany commentParser) <|> try (skipMany eol) )
	try (skipMany space)
	char ')'
	return $List $ filter (not . isComment) x

dottedListParser :: Parser Primitives
dottedListParser = do
	char '('
	try (skipMany space)
    	head <- sepEndBy exprParser spaces -- <|> try (skipMany commentParser) <|> try (skipMany eol) )
    	tail <- char '.' >> spaces >> exprParser
	try (skipMany space)
    	char ')'
    	return $ DottedList (filter (not . isComment) head) tail

quotedParser :: Parser Primitives
quotedParser = do
    char '\''
    x <- exprParser
    return $ List [Atom "quote", x]

quasiQuotedParser :: Parser Primitives
quasiQuotedParser  = do
     char '`'
     x <- exprParser
     return $ List [Atom "quasiquote", x]
unQuoteParser :: Parser Primitives
unQuoteParser = do
     char ','
     x <- exprParser
     return $ List [Atom "unquote", x]

myeof :: Parser String
myeof = do
	eof
	return "\n"

commentParser :: Parser Primitives
commentParser = do 
	char ';'
	--skipMany $ letter <|> symbol <|> digit <|> space
	manyTill anyChar (try eol<|> myeof)
	return $Comment ";"


exprParser =
	try commentParser
	<|> try negComplexParser
	<|> try negRealParser
	<|> try negRatioParser
	<|> try negDigitParser
	<|> atomParser
	<|> stringParser
	<|> try complexParser
	<|> try realParser
	<|> try ratioParser
	<|> try numberParser
	<|> try boolParser
	<|> quotedParser
	{-<|> do 
	- 	char '('
	       x <- try listParser <|> dottedListParser
	       --try $skipMany commentParser
	       --try $skipMany eol
	       --char ')'
	       return x-}
	<|> try listParser
	<|> dottedListParser
	<|> try quasiQuotedParser
       	<|> try unQuoteParser
	<?> "scheme expression"



