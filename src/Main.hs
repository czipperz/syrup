module Main where

import qualified Lex
import Pos
import qualified Parse
import System.IO
import Control.Monad ((<=<))

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  line <- getLine
  runLine line
  loop

runLine :: String -> IO ()
runLine = putStrLn . show . (Parse.parseStatements <=< Lex.tokens "*stdin*")

main :: IO ()
main = do
  putStrLn "Cerp interactive prompt"
  loop
