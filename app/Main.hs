{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
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
      Parser, try, anyChar, notFollowedBy, alphaNum, sepBy, endBy)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

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
parseAtom = liftM Atom $ (letter <|> symbol) >> many (letter <|> digit <|> symbol)

parseBool :: Parser LispVal
parseBool = (try (string "#t")>> notFollowedBy alphaNum >> return (Bool True)) <|>
            (try (string "#f") >> notFollowedBy alphaNum >> return (Bool False))

parseNumberWithPrefix :: Parser LispVal
parseNumberWithPrefix = (string "#d" >> many1 digit >>= \x -> (return . Number . read) x) <|>
                        (string "#o" >> many1 digit >>= \x -> (return . Number . fst . head . readOct) x) <|>
                        (string "#h" >> many1 digit >>= \x -> (return . Number . fst . head . readHex) x) <|>
                        (string "#b" >> many1 digit >>= \x -> (return . Number . fst . head . readBin) x)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit) <|> try parseNumberWithPrefix

parseChar :: Parser LispVal
parseChar = do
            try $ string "#\\"
            c <- try (string "space") <|> try (string "newline") <|> try (do{x <- anyChar; notFollowedBy alphaNum; return [x]}) 
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
    real <- many1 digit
    char '+'
    img <- many1 digit
    char 'i'
    (return . Complex) (read real + read img)

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
