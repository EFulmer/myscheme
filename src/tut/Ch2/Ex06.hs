-- Exercise 6:
-- Add a Float constructor to LispVal, and support R5RS syntax for decimals. 
-- The Haskell function readFloat may be useful.

module Main where
import Control.Monad
import Data.Char
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character String
             | Float Float
             | String String
             | Bool Bool
            deriving Show


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


parseBool :: Parser LispVal
parseBool = do
                -- the try combinator is backtracking; since numbers
                -- can have base prefixes starting with #, we use this in 
                -- order to backtrack on a #d/etc., so it can be properly
                -- parsed as a number.
                val <- try (string "#t" <|> string "#f")
                case val of "#t" -> return $ Bool True
                            "#f" -> return $ Bool False


parseChar :: Parser LispVal
parseChar = do
                try $ string "\\#"
                -- either:
                -- single alphanum followed by delim (space, paren),
                -- alphanum string followed by delim
                -- single other symbol
                val <- alphaNum <|> bracket
                return $ Character val
                where bracket = string "(" <|> string ")" <|> string "[" 
                            <|> string "]" <|> string "{" <|> string "}"
                      alphaNum = many1 $ letter <|> digit


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapeChar <|> (noneOf "\"")
                char '"'
                return $ String x
                where escapeChar = do
                                    char '\\'
                                    x <- oneOf "\\\"nrtb"
                                    return $ case x of
                                        '\\' -> '\\'
                                        '"'  -> '"'
                                        'n'  -> '\n'
                                        'r'  -> '\r'
                                        't'  -> '\t'
                                        'b'  -> '\b'


parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom


-- do-notation:
parseNumberDo :: Parser LispVal
parseNumberDo = do
            x <- many1 digit
            let numVal = (Number . read) x
            return numVal


parseNumberBase :: Parser LispVal
parseNumberBase = do
            radix <- option 'd' parseRadix
            x <- case radix of 
                'b' -> many1 (oneOf "01")
                'o' -> many1 octDigit
                'd' -> many1 digit
                'x' -> many1 hexDigit
            return $ Number $ case radix of
                'b' -> readBin x
                'o' -> fst $ (readOct x) !! 0
                'd' -> read x
                'x' -> fst $ (readHex x) !! 0
            where parseRadix = char '#' >> (oneOf "bodx")
                  -- TODO figure out a nicer way to get an Integer from a
                  -- String
                  readBin b = sum [ 2 ^ col * (read [val] :: Integer) 
                                    | (col, val) <- zip [0..] (reverse b) ]


parseFloating :: Parser LispVal
parseFloating = do
            -- According to R5RS, Scheme floating point numbers only need 
            -- to have one part; i.e. 1. and .1 are both valid floating 
            -- point numbers in the grammar.
            -- readFloat however expects the integer part to ALWAYS be given.
            float <- try intOpt <|> try fracOpt
            return $ Float $ fst $ (readFloat float) !! 0
            where
                intOpt :: Parser String
                intOpt = do
                    i <- option "0" (many1 digit)
                    char '.'
                    f <- many1 digit 
                    let n = i ++ "." ++ f
                    return n
                fracOpt :: Parser String
                fracOpt = do
                    i <- many1 digit
                    char '.'
                    f <- option "0" (many1 digit)
                    let n = i ++ "." ++ f
                    return n


-- explicit sequencing with the >>= operator
parseNumberBind :: Parser LispVal
parseNumberBind = (many1 digit) >>= (\x -> return ((Number . read) x))


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseBool <|> parseString <|> parseFloating <|> parseNumberBase


spaces :: Parser ()
spaces = skipMany1 space


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
