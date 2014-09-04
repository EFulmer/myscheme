-- Exercise 2
-- Change unpackNum so that it always returns 0 if the value is not a number, 
-- even if it's a string or list that could be parsed as a number.

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

instance Show LispVal where show = showVal


showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number n)             = show n
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " 
                                 ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


spaces :: Parser ()
spaces = skipMany1 space


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
-- parseNumberDo :: Parser LispVal
-- parseNumberDo = do
--             x <- many1 digit
--             let numVal = (Number . read) x
--             return numVal


parseInt :: Parser LispVal
parseInt = do
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
-- parseNumberBind :: Parser LispVal
-- parseNumberBind = (many1 digit) >>= (\x -> return ((Number . read) x))


-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber :: Parser LispVal
parseNumber = parseInt <|> parseFloating


-- orig. was sepBy parseExpr spaces, but this way is a little more intuitive:
-- sepBy is a combinator that parses multiple occurrences of the first parser, 
-- each separated by the second. 
-- It returns the results of the first parser in a list.
parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces


-- endBy is basically sepBy, except the second parser also has to END the 
-- sequence of occurrences of the first.
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    -- mixing up >>= and >> with do notation; this is shorter than 
    -- char '.' as their own lines, each w/o bindings
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


-- 'blah is syntactic sugar for (quote blah)
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom 
         <|> parseBool 
         <|> parseString 
         <|> parseFloating 
         <|> parseInt
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val


eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args


-- Because it took me a little bit to get, here are some notes on how
-- maybe (the builtin function, not the Maybe type) work.
--
-- maybe takes three arguments:
-- an (a -> b) function,
-- a Maybe value of the source type (a),
-- and an "unwrapped" value of the result type (b).
-- If the Maybe contains an actual value, it applies the function, else it 
-- returns the default value of the result type.
--
-- Another cool thing going on here: ($ args) means that the "missing" 
-- argument will be applied to args; (args $) would mean that args 
-- is being applied, which is obviously not what we want.
--
-- Maybe I'll play around with this later by trying to rearrange the 
-- arguments here into another form that also works.
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", boolUnop isNum),
              ("bool?", boolUnop isBool),
              ("pair?", boolUnop isPair),
              ("null?", boolUnop isNull),
              ("list?", boolUnop isList),
              ("symbol?", boolUnop isSym),
              ("char?", boolUnop isChar),
              ("string?", boolUnop isString)]


-- this is hacky and I dislike it
boolUnop :: (LispVal -> LispVal) -> [LispVal] -> LispVal
boolUnop f [b] = f b
boolUnop _ _   = undefined


isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _        = Bool False


isList :: LispVal -> LispVal
isList (List _)             = Bool True
isList _                    = Bool False


isPair :: LispVal -> LispVal
isPair (DottedList _ _) = Bool True
isPair _                = Bool False


isNull :: LispVal -> LispVal
isNull (List l) = Bool $ null l
isNull _        = Bool False


isSym :: LispVal -> LispVal
isSym (Atom _) = Bool True
isSym _        = Bool False


isChar :: LispVal -> LispVal
isChar (Character _) = Bool True
isChar _             = Bool False


isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False


-- TODO once vectors are implemented
isVector :: LispVal -> LispVal
isVector _ = undefined


-- TODO expand with numeric types once full tower is implemented
isNum :: LispVal -> LispVal
isNum (Number _) = Bool True
isNum (Float _)  = Bool True
isNum _          = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


-- TODO in "my" interpreter, remove the weak typing here (everything aside
-- first pattern), replace with sensible error handling.
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0


main :: IO ()
-- main = getArgs >>= (print . eval . readExpr . head)
main = do
    args <- getArgs
    putStrLn (showVal (readExpr (args !! 0)))
