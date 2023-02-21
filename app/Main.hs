module Main where

import Data.Complex (Complex ((:+)))
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Numeric (readOct, readHex, readBin, readFloat)
import Text.Parsec.Token (GenTokenParser(decimal))

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | Complex (Complex Double)
            | Rational Integer Integer
            | Float Float
            | Char Char
            | String String
            | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- #######################################
-- Atom
-- ######################################
parseAtom :: Parser LispVal
parseAtom =
    do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom

-- #####################################
-- Bool
-- #####################################
parseBool :: Parser LispVal
parseBool =
    do
        try $ char '#'
        (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- #####################################
-- Numeric
-- #####################################
oct2dig :: String -> Integer
oct2dig = fst . head . readOct

hex2dig :: String -> Integer
hex2dig = fst . head . readHex

bin2dig :: [Char] -> Integer
bin2dig = fst . head . readBin 

parseDec1 :: Parser LispVal
parseDec1 = 
    do
        x <- many1 digit
        return $ Number (read x)

parseDec2 :: Parser LispVal
parseDec2 =
    do
        try $ string "#d"
        x <- many1 digit
        return $ Number (read x)

parseOct :: Parser LispVal
parseOct = 
    do
        try $ string "#o"
        x <- many1 octDigit
        return $ Number (oct2dig x)

parseHex :: Parser LispVal
parseHex =
    do
        try $ string "#x"
        x <- many1 hexDigit
        return $ Number (hex2dig x)

parseBin :: Parser LispVal
parseBin =
    do
        try $ string "#b"
        x <- many1 (oneOf "01")
        return $ Number (bin2dig x)

parseNumber :: Parser LispVal
parseNumber = parseDec1 
            <|> parseDec2
            <|> parseOct
            <|> parseHex
            <|> parseBin

-- ######################################
-- Complex
-- ######################################
parseRational :: Parser LispVal
parseRational =
    do
        x <- many1 digit
        char '/'
        y <- many1 digit
        return $ Rational (read x) (read y)

-- #####################################
-- Rational
-- #####################################
toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromInteger n

parseComplex :: Parser LispVal
parseComplex =
    do
        x <- try parseFloat <|> try parseDec1
        char '+'
        y <- try parseFloat <|> try parseDec1
        return $ Complex (toDouble x :+ toDouble y)

-- ######################################
-- Float
-- ######################################
parseFloat :: Parser LispVal
parseFloat =
    do
        x <- many1 digit
        char '.'
        y <- many1 digit
        return $ Float (fst . head $ readFloat (x ++ "." ++ y))

-- ######################################
-- String
-- ######################################
parseStringCharacter :: Parser Char
parseStringCharacter = noneOf "\\\""

parseStringEscaped :: Parser Char
parseStringEscaped = 
    do
        char '\\'
        x <- oneOf "\\\"nrt"
        return $ case x of
                '\\' -> x
                '"'  -> x
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

parseStringInner :: Parser Char
parseStringInner = parseStringEscaped 
                <|> parseStringCharacter 

parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many parseStringInner
        char '"'
        return $ String x

-- #########################################
-- Char
-- #########################################
parseCharNewline :: Parser Char
parseCharNewline =
    do
        try $ string "newline"
        return '\n'

parseCharSpace :: Parser Char
parseCharSpace =
    do
        try (string "space" <|> string " ")
        return ' '

parseCharAny :: Parser Char
parseCharAny =
    do
        x <- anyChar
        notFollowedBy alphaNum
        return x

parseChar :: Parser LispVal
parseChar =
    do
        try $ string "#\\"
        x <- parseCharNewline <|> parseCharSpace <|> parseCharAny
        return $ Char x

-- #########################################
-- Expr
-- #########################################
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseChar
        <|> try parseFloat
        <|> parseNumber
        <|> parseBool

readExpr :: String -> String
readExpr input = 
    case parse parseExpr "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value " ++ case val of
            String x -> "String \"" ++ x ++ "\""
            Char x -> "Char '" ++ [x] ++ "'"
            Number x -> "Number " ++ show x
            Float x -> "Float " ++ show x
            Bool x -> "Bool " ++ show x
            _ -> "UNKNOWN"

main :: IO ()
main = 
    do
        (expr:_) <- getArgs
        putStrLn $ readExpr expr