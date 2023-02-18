module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber :: Parser LispVal
-- parseNumber =
--     do
--         x <- many1 digit
--         (return . Number . read) x

-- parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= return . Number . read

parseStringCharacter :: Parser Char
parseStringCharacter = noneOf "\\\""

parseStringEscaped :: Parser Char
parseStringEscaped = 
    do
        char '\\'
        x <- oneOf "\"\\nrt"
        return $ case x of
                    '"'  -> '"'
                    '\\' -> '\\'
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'
                    _    -> ' '

parseStringInner :: Parser Char
parseStringInner = parseStringCharacter 
                <|> parseStringEscaped

parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many parseStringInner 
        char '"'
        return $ String x

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = 
    case parse (spaces >> parseExpr) "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value"

main :: IO ()
main = 
    do
        (expr:_) <- getArgs
        putStrLn $ readExpr expr