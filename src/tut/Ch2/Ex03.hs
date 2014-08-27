-- Exercise 3:
-- Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters

module Main where
import Control.Monad
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


-- Update: addition of the <|> (char '\\' >> char '"') bit
-- (char '\\' >> char '"') will parse a backslash followed by a double-quote,
-- returning the double-quote.
--
-- >> discards the left-hand parser's result in the Parser monad.
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


-- explicit sequencing with the >>= operator
parseNumberBind :: Parser LispVal
parseNumberBind = (many1 digit) >>= (\x -> return ((Number . read) x))


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumberBind


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
