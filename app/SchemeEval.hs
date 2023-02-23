module SchemeEval where

import Scheme

import GHC.Arr (Array, elems)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Float float) = show float
showVal (Complex complex) = show complex
showVal (Rational n d) = show d ++ "/" ++ show d
showVal (Char c) = [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector v) = "#(" ++ unwordsList (elems v) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal