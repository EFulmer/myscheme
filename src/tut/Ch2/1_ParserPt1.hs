module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


-- symbol tries to parse the next character, succeeding if it's one 
-- of the arguments passed to oneOf
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- how parse works:
-- first arg = a parser, 
-- second = filepath (in a "real" parser we'd have the file's name here 
-- or something); not actually used except to show error messages
-- third = text to be parsed
--
-- It returns an Either ParseError a 
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
