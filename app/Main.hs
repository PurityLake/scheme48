module Main
  ( main
  ) where

import           SchemeParser
import           System.Environment (getArgs)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
