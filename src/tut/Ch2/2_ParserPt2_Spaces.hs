module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


-- symbol tries to parse the next character, succeeding if it's one 
-- of the arguments passed to oneOf
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- spaces parses whitespace.
-- skipMany1 takes a parser and applies it 1+ times (like the + operator in 
-- regex)
spaces :: Parser ()
spaces = skipMany1 space

-- how parse works:
-- first arg = a parser (here we're doing space >> symbol, 
-- to parse spaces, then a symbol - this fails if either fails, since 
-- the Parsec parsers are monads. 
-- In other words, the "!$#" etc characters need to be preceded by at 
-- least one space.)
--
-- second = filepath (in a "real" parser we'd have the file's name here 
-- or something); not actually used except to show error messages
--
-- third = text to be parsed
--
-- It returns an Either ParseError a 
readExpr :: String -> String
readExpr input = case parse (space >> symbol) "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
