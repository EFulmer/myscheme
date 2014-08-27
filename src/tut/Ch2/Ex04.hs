-- Exercise 4:


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
             | String String
             | Bool Bool
            deriving Show


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


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
            radix <- option 'd' radix
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
            where radix = char '#' >> (oneOf "bodx")
                  -- TODO figure out a nicer way to get an Integer from a
                  -- String
                  readBin b = sum [ 2 ^ col * (read [val] :: Integer) 
                                    | (col, val) <- zip [0..] (reverse b) ]


-- explicit sequencing with the >>= operator
parseNumberBind :: Parser LispVal
parseNumberBind = (many1 digit) >>= (\x -> return ((Number . read) x))


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumberBase


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
