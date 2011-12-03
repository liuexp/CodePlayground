module ScmShow where
import Parser.Primitives
import Numeric
import Ratio
import Complex
import ScmError

showVal (Comment contents) = "" --"; <comment>" 
showVal (Verbatim contents) = contents 
showVal (String contents) = "\""++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Real contents) = show contents
showVal (Ratio contents) 
	|(denominator contents) /= 1 = show (numerator contents) ++ "/" ++ show (denominator contents)
	|otherwise  = show (numerator contents)
showVal (Complex contents) = 
	show (realPart contents) ++ signop ++ show (imagPart contents) ++ "i" 
	where signop = if (imagPart contents) >= 0
			  then "+"
			  else ""
	{-where signop
	      |(imagPart contents) >=0 = "+"
	      |otherwise = ""
	 -}

showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveProc _) = "<Predefined global procedure>"
showVal (Procedure {params = args, vararg = varargs, body = body, closure = env}) = 
  "<compound procedure>: (lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)" 
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
{-
showNum (IntNumber contents) = show contents
showNum (Real contents) = show contents
showNum (Ratio contents) 
	|(denominator contents) /= 1 = show (numerator contents) ++ "/" ++ show (denominator contents)
	|otherwise  = show (numerator contents)
showNum (Complex contents) = show (realPart contents) ++"+" ++ show (imagPart contents) ++ "i"
-}
showError :: ScmError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default str) = str





unwordsList = unwords . map showVal


instance Show ScmError where show = showError
instance Show Primitives where show = showVal
--instance Show ScmNum where show = showNum








