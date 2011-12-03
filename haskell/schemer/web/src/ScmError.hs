module ScmError where
import Control.Monad.Error
import Parser.Primitives
import Text.ParserCombinators.Parsec hiding (spaces)
instance Error ScmError where
     noMsg = Default "An error has occurred"
     strMsg = Default


trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
