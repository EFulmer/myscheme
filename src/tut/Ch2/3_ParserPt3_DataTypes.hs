module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


-- The String and Bool constructors are allowed because type names and 
-- type constructors/tags live in separate namespaces.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


-- symbol tries to parse the next character, succeeding if it's one 
-- of the arguments passed to oneOf
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- Parse a double quote (char '"'),
-- then any number of non-double quote characters (many (noneOf "\""))
-- ; many takes a parser and applies it 0+ times; 0+ since the empty string
-- is a valid -- but boring -- string,
-- then parse another double quote (char '"').
--
-- Finally, return the resultant string between the double quotes as a 
-- LispVal -- a String.
--
-- It's all wrapped up in a do-block because Parsec parsers are monads; 
-- like Either types because parsing can fail at any given point.
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x


-- This isn't too different from the above; the main addition is <|>.
-- <|> tries its left-hand argument, and if it fails, tries the 
-- right-hand argument. If the left parser fails, no input is consumed.
--
-- First grab a letter or a symbol (#, %, etc), then grab any number of
-- letters, digits, or symbols (#t, %3why, t2, #winning, and $ are all
-- kosher), then return a True LispVal if "#t" is found, False if "#f" is
-- found, otherwise just the entire parsed string as an Atom.
--
-- The case at the end is a good example of case used as an expression.
parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom


-- many1 digit just parses one or more digits in sequence.
-- (Number . read) takes a String representation of a LispVal and returns 
-- it as a Number (again, a type of LispVal)
-- and liftM lifts a regular (a -> b) function into a particular Monad;
-- (m a -> m b); the type signature here indicates that we're turning 
-- the rest of this into a function in the Parser monad.
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


-- This one is actually simple; just try and parse an Atom, if that fails, 
-- try and parse a String, and if that fails, try and parse a Number.
-- If all three fail, just give up and throw an error message.
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber


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
-- Its return type is Either ParseError a - the Left ParseError is 
-- returned when the parser failed; the ParseError is a String 
-- describing the error, where it was found, etc etc.
-- The Right a is just whatever the parser function returns on success.
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
