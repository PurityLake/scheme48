module Scheme where

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