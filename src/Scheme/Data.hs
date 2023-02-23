module Scheme.Data
  ( LispVal(..)
  ) where

import           Data.Complex (Complex)
import           GHC.Arr      (Array, elems)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)
  | Number Integer
  | Complex (Complex Double)
  | Rational Integer Integer
  | Float Float
  | Char Char
  | String String
  | Bool Bool

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Float float) = show float
showVal (Complex complex) = show complex
showVal (Rational n d) = show n ++ "/" ++ show d
showVal (Char c) = [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList listhead listtail) =
  "(" ++ unwordsList listhead ++ " . " ++ showVal listtail ++ ")"
showVal (Vector v) = "#(" ++ unwordsList (elems v) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
