module SchemeParser where

import Scheme
import Data.Complex (Complex ((:+)))
import GHC.Arr (Array, listArray)
import Numeric (readBin, readFloat, readHex, readOct)
import Text.Parsec.Token (GenTokenParser (decimal))
import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- #######################################
-- Atom
-- ######################################
parseAtom :: Parser LispVal
parseAtom =
  do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ Atom atom

-- #####################################
-- Bool
-- #####################################
parseBool :: Parser LispVal
parseBool =
  do
    (char 't' >> return (Bool True)) 
      <|> (char 'T' >> return (Bool True))
      <|> (char 'f' >> return (Bool False))
      <|> (char 'F' >> return (Bool False))

-- #####################################
-- Number
-- #####################################
oct2dig :: String -> Integer
oct2dig = fst . head . readOct

hex2dig :: String -> Integer
hex2dig = fst . head . readHex

bin2dig :: [Char] -> Integer
bin2dig = fst . head . readBin

parseDec :: Parser LispVal
parseDec =
  do
    char 'd'
    x <- many1 digit
    return $ Number (read x)

parseOct :: Parser LispVal
parseOct =
  do
    char 'o'
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseHex :: Parser LispVal
parseHex =
  do
    char 'x'
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseBin :: Parser LispVal
parseBin =
  do
    char 'b'
    x <- many1 (oneOf "01")
    return $ Number (bin2dig x)

parseNumber :: Parser LispVal
parseNumber =
  do
    x <- many1 digit
    return $ Number (read x)

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
    x <- try parseFloat <|> try parseNumber
    char '+'
    y <- try parseFloat <|> try parseNumber
    char 'i'
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
      '"' -> x
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'

parseStringInner :: Parser Char
parseStringInner =
  parseStringEscaped
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
    string "newline"
    return '\n'

parseCharSpace :: Parser Char
parseCharSpace =
  do
    string "space" <|> string " "
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
    char '\\'
    x <- parseCharNewline <|> parseCharSpace <|> parseCharAny
    return $ Char x

-- #########################################
-- Lists
-- #########################################
parseList :: Parser LispVal
parseList =
  do
    char '(' >> spaces
    head <- parseExpr `sepEndBy` spaces1
    do
      char '.' >> spaces1
      tail <- parseExpr
      spaces >> char ')'
      return $ DottedList head tail
      <|> (spaces >> char ')' >> return (List head))

parseQuoted :: Parser LispVal
parseQuoted =
  do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing =
  do
    string ",@"
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseUnquote :: Parser LispVal
parseUnquote =
  do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted =
  do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

-- ##########################################
-- Vector
-- ##########################################
parseVector :: Parser LispVal
parseVector =
  do
    char '('
    x <- parseVector'
    char ')'
    return x

parseVector' :: Parser LispVal
parseVector' =
  do
    arrayValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0, length arrayValues - 1) arrayValues)

-- ##########################################
-- Numeric
-- ##########################################
parseNumeric :: Parser LispVal
parseNumeric =
  try parseComplex
    <|> try parseFloat
    <|> try parseRational
    <|> parseNumber

parseHash :: Parser LispVal
parseHash =
  do
    char '#'
    parseBin 
      <|> parseBool
      <|> parseChar
      <|> parseDec
      <|> parseHex
      <|> parseOct
      <|> parseVector

-- #########################################
-- Expr
-- #########################################
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumeric
    <|> parseQuoted
    <|> parseHash
    <|> parseList
    <|> parseQuasiQuoted
    <|> parseUnquote
    <|> parseUnquoteSplicing

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val ->
      "Found value " ++ case val of
        String x -> "String \"" ++ x ++ "\""
        Char x -> "Char '" ++ [x] ++ "'"
        Number x -> "Number " ++ show x
        Float x -> "Float " ++ show x
        Bool x -> "Bool " ++ show x
        Complex x -> "Complex " ++ show x
        Rational x y -> "Rational " ++ show x ++ "/" ++ show y
        Vector _ -> "Vector"
        List _ -> "List"
        DottedList _ _ -> "Dotted List"