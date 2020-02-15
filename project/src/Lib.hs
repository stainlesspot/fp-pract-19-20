module Lib where

import Prolog.Program (Program)
import Prolog.ProgramParser (programParser, goalParser)
import Prolog.Goal (resolve)
import Parser (parse)
import System.Environment
import System.IO
import Data.List
import Control.Monad (forever)

printPartialHelper :: Show a => [a] -> IO ()
printPartialHelper []     = pure ()
printPartialHelper (x:xs) = do
  print x
  c <- getChar
  if c == ' ' || c == ';'
     then printPartial xs
     else pure ()

printPartial :: Show a => [a] -> IO ()
printPartial xs = do
  hSetBuffering stdin NoBuffering
  printPartialHelper xs
  hSetBuffering stdin LineBuffering

repl :: Program -> IO ()
repl program = forever $ do
  putStr "?- "
  hFlush stdout
  inputGoal <- getLine
  case parse goalParser inputGoal of
    Nothing -> putStrLn "Not a valid goal."
    Just [] -> putStrLn "false."
    Just as -> printPartial $ resolve program as

parseFile :: String -> IO ()
parseFile path = do
  contents <- readFile path
  case parse programParser contents of
    Nothing -> putStrLn "Not a valid prolog program."
    Just p  -> repl p

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Program not provided."
    (progPath : _) -> parseFile progPath
