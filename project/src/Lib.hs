module Lib where

import Prolog.Repl (repl)
import Prolog.ProgramParser (programParser)
import Parser (parse)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

parseFile :: String -> IO ()
parseFile path = do
  contents <- readFile path
  case parse programParser contents of
    Nothing -> hPutStrLn stderr "Not a valid prolog program."
    Just p  -> repl p

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hPutStrLn stderr "Program not provided."
    (progPath : _) -> parseFile progPath
