{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Text.ParserCombinators.Parsec
    ( char,
      digit,
      letter,
      string,
      noneOf,
      oneOf,
      space,
      many1,
      skipMany1,
      (<|>),
      many,
      parse,
      Parser, ParseError, try, anyChar, notFollowedBy, alphaNum, sepBy, endBy)
import System.Environment ( getArgs )
import Control.Monad ( liftM )
import Numeric ( readBin, readFloat, readHex, readOct )
import Data.Ratio ( (%) )
import Data.Complex ( Complex(..) )
import Control.Monad.Except

main :: IO ()
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Double
             | Rational Rational
             | Complex (Complex Double)

escapedChar :: Parser Char
escapedChar = do
                char '\\'
                x <- oneOf "\\\"rnt"
                return $ case x of
                    'r' -> '\r'
                    'n' -> '\n'
                    't' -> '\t'
                    '"' -> x
                    '\\' -> x

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChar <|> noneOf "\\\"\r\n\t"
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = (try (string "#t")>> notFollowedBy alphaNum >> return (Bool True)) <|>
            (try (string "#f") >> notFollowedBy alphaNum >> return (Bool False))

parseNumberWithPrefix :: String -> Parser LispVal
parseNumberWithPrefix prefix = do
            string prefix
            x <- many1 digit
            (return . Number) $ case prefix of
                "#d" -> read x
                "#o" -> (fst . head . readOct) x
                "#h" -> (fst . head . readHex) x
                "#b" -> (fst . head . readBin) x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)
            <|> try (parseNumberWithPrefix "#d")
            <|> try (parseNumberWithPrefix "#o")
            <|> try (parseNumberWithPrefix "#h")
            <|> try (parseNumberWithPrefix "#b")

parseChar :: Parser LispVal
parseChar = do
            try $ string "#\\"
            c <- try (string "space") <|> try (string "newline") <|> try (do {x <- anyChar; notFollowedBy alphaNum; return [x]})
            (return . Char) $ case c of
                "space" -> ' '
                "newline" -> '\n'
                otherwise -> head c

parseFloat :: Parser LispVal
parseFloat = try $ do
        x1 <- many1 digit
        char '.'
        x2 <- many1 digit
        (return . Float . fst . head . readFloat) (x1 ++ "." ++ x2)

parseRational :: Parser LispVal
parseRational = try $ do
        numer <- many1 digit
        char '/'
        denom <- many1 digit
        (return . Rational) (read numer % read denom)

parseComplex :: Parser LispVal
parseComplex = try $ do
    real <- try parseFloat <|> parseNumber
    char '+'
    img <- try parseFloat <|> parseNumber
    char 'i'
    (return . Complex) (toDouble real :+ toDouble img)

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

{- TODO: Quasiquotations, Vectors -}
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseRational
    <|> parseComplex
    <|> parseFloat
    <|> parseNumber
    <|> parseBool
    <|> parseChar
    <|> parseQuoted
    <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char contents) = "\'" ++ [contents] ++ "\'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show :: LispVal -> String
                            show = showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Char _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Rational _) = return val
eval val@(Complex _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred , conseq, alt]) = do
            result <- eval pred
            case result of
                Bool False -> eval alt
                otherwise -> eval conseq
eval (List [Atom "cond"]) = return $ Bool False
eval (List [Atom "cond", List (Atom "else":expr)]) = do 
                mapM_ eval $ init expr
                eval $ last expr
eval (List (Atom "cond":List (test:expr):xss)) = do
            result <- eval test
            case result of
                Bool True -> do
                    mapM_ eval $ init expr
                    eval $ last expr
                otherwise -> eval $ List (Atom "cond":xss)
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive Function" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("boolean?", unaryOp booleanp),
              ("symbol?", unaryOp symbolp),
              ("list?", unaryOp listp),
              ("symbol", unaryOp symbol2string),
              ("string", unaryOp string2symbol),
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
              ("cons", cons),
              ("cdr", cdr),
              ("eqv?", eqv),
              ("eq?", eqv)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [x1, x2] = do
                            left <- unpacker x1
                            right <- unpacker x2
                            (return . Bool) $ op left right
boolBinop unpacker _ badArg = throwError $ NumArgs 2 badArg

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp op [param] = return $ op param

stringp, numberp, booleanp, symbolp, listp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp otherwise = Bool False
numberp (Number _) = Bool True
numberp otherwise = Bool False
booleanp (Bool _) = Bool True
booleanp otherwise = Bool False
symbolp (Atom _) = Bool True
symbolp otherwise = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp otherwise = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x :xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show :: LispError -> String
                              show = showError

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val









