-- Exercise 2:
-- Our strings aren't quite R5RS compliant, because they don't support 
-- escaping of internal quotes within the string. Change parseString so that 
-- \" gives a literal quote character instead of terminating the string. 
-- You may want to replace noneOf "\"" with a new parser action that accepts 
-- either a non-quote character or a backslash followed by a quote mark.

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



parseString :: Parser LispVal
parseString = do
                char '"'
                x <- (many ((noneOf "\"") <|> (char '\\' >> oneOf "\\\"")))
                char '"'
                return $ String x


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
    Right val -> "Found value" ++ show val


main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
