-- TODO remove this if it's not in actual Scheme...? (weak typing)
-- In DrRacket (= "2" 2) is #f, same for repl.it's Scheme (BiwaScheme)
{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Control.Monad
import Control.Monad.Error
import Data.Char
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


-- Begin data type and typeclass derivation definitions:

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


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                        ++ " args; found values " 
                                        ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


instance Show LispError where show = showError


-- Some notes on how Error works:
-- noMsg is the default error to throw. No message, or a very generic one.
-- strMsg takes a string and "lifts" it into the particular Error instance,
-- following whatever behavior that values in that instance of Error 
-- should use.
instance Error LispError where 
    noMsg = Default "An error occurred"
    strMsg = Default


-- We're making a monad out of LispError here. (remember, Either a is a monad;
-- where a is a concrete type, Either a is a monad parameterized over all 
-- poss. b)
-- In other words:
-- ThrowsError = Either LispError a
--
-- MonadError is another typeclass, where the error is handled inside of
-- some monad. 
-- It has two functions; throwError lifts an Error value into its monad, 
-- making it a Left value (since that represents error).
--
-- catchError takes an Either value and a function for handling;
-- it's sort of a reverse of bind; while bind (>>=) typically 
-- (for List, Either, and Maybe at least) transforms a "success" value 
-- using the function it's given and leaves a "failure" value alone, 
-- catchError transforms a failure value and passes a success value 
-- along unchanged.
type ThrowsError = Either LispError


-- TODO remove this if it's not in actual Scheme...? (weak typing)
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


-- End type definitions
-- Begin parser definitions


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
                val <- try (string "#t") <|> try (string "#f")
                return $ Bool $ val == "#t"


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


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err -- lifting it
    Right val -> return val


-- PARSER ENDS HERE --
-- EVALUATOR BEGINS HERE --


-- note: mapM is just map where the higher order func's return type is monadic
eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                        = return val
eval val@(Number _)                        = return val
eval val@(Bool _)                          = return val
eval (List [Atom "quote", val])            = return val
eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                    Bool False -> eval alt
                                                    otherwise  -> eval conseq
eval (List (Atom func : args))             = mapM eval args >>= apply func
eval badForm                               = throwError $ BadSpecialForm 
                                "Unrecognized special form" badForm


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
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args) 
                        (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", unaryOp isNum),
              ("bool?", unaryOp isBool),
              ("pair?", unaryOp isPair),
              ("null?", unaryOp isNull),
              ("list?", unaryOp isList),
              ("symbol?", unaryOp isSym),
              ("char?", unaryOp isChar),
              ("string?", unaryOp isString),
              ("symbol->string", unaryOp symToStr),
              ("string->symbol", unaryOp strToSym),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- TODO: Make the type-checking stricter here.
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool


-- TODO rename this to reflect that it works for the typechecking ops
-- or something?
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [b] = return $ op b
unaryOp _ []   = throwError $ NumArgs 1 []
unaryOp _ bs   = throwError $ NumArgs 1 bs


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


-- Numeric operations:
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op


-- TODO in "my" interpreter, remove the weak typing here (everything aside
-- first pattern), replace with sensible error handling.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


-- symbol->string
symToStr :: LispVal -> LispVal
symToStr (Atom v) = String v
symToStr _        = undefined


-- string->symbol
strToSym :: LispVal -> LispVal
strToSym (String v) = Atom v
strToSym _          = undefined


-- List stuff:
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]          = return $ List [x]
cons [x, List xs]          = return $ List (x:xs)
cons [x, DottedList xs x'] = return $ DottedList (x:xs) x'
cons [x1, x2]              = return $ DottedList [x1] x2
cons badArgList            = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

eqvPair (x1, x2) = case eqv [x1, x2] of
    Left err -> False
    Right (Bool val) -> val


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do unpacked1 <- unpacker arg1
                                                   unpacked2 <- unpacker arg2
                                                   return $ unpacked1 == unpacked2 
                                                   `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, 
                             AnyUnpacker unpackBool]
                        eqvEquals <- eqv [arg1, arg2]
                        return $ Bool $ (primitiveEquals || 
                                         let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
                

-- End primitive definitions
-- Begin error handling definitions


trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val


main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError $ evaled
