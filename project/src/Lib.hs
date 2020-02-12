module Lib where

import System.Environment
import System.IO
import Data.List
import Control.Monad (forever)
import Prolog.DataTypes (Program)
import Prolog.ProgramParser
import Prolog.Goal
import Parser (parse)

repl :: Program -> IO ()
repl program = forever $ do
  putStr "?- "
  hFlush stdout
  inputGoal <- getLine
  case resolve program =<< parse goalParser inputGoal of
    Nothing  -> putStrLn "false."
    Just sub -> print sub

parseFile :: String -> IO ()
parseFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  case parse programParser contents of
    Nothing -> putStrLn "Not a valid prolog program."
    Just p  -> repl p

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Program not provided."
    (progPath : _) -> parseFile progPath
