module SchemeEval where

import Scheme

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number number) = show number
showVal (Float float) = show float
showVal (Complex complex) = show complex
showVal (Rational n d) = show d ++ "/" ++ show d
