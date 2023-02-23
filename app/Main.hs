module Main (main) where

import System.Environment (getArgs)
import SchemeParser

main :: IO ()
main =
  do 
    (expr : _) <- getArgs
    putStrLn $ readExpr expr