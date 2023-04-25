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
      Parser, try, anyChar, notFollowedBy, alphaNum)
import System.Environment
import Control.Monad
import Numeric

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
parseNumberWithPrefix = try (string "#d" >> many1 digit >>= \x -> (return . Number . read) x) <|>
                        try (string "#o" >> many1 digit >>= \x -> (return . Number . fst . head . readOct) x) <|>
                        try (string "#h" >> many1 digit >>= \x -> (return . Number . fst . head . readHex) x) <|>
                        try (string "#b" >> many1 digit >>= \x -> (return . Number . fst . head . readBin) x)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit) <|> parseNumberWithPrefix

parseChar :: Parser LispVal
parseChar = do
            try $ string "#\\"
            c <- try (string "space") <|> try (string "newline") <|> try (do{x <- anyChar; notFollowedBy alphaNum; return [x]}) 
            (return . Char) $ case c of
                "space" -> ' '
                "newline" -> '\n'
                otherwise -> head c

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool <|> parseChar