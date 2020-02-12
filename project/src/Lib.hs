module Lib where

import System.Environment
import System.IO
import Data.List
import Control.Monad (forever)
import Prolog.ProgramParser
import Prolog.Goal
import Parser (parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Program not provided."
    (progPath : _) -> do
      handle <- openFile progPath ReadMode
      contents <- hGetContents handle
      case parse programParser contents of
        Nothing      -> putStrLn "Not a valid prolog program."
        Just program -> forever $ do
          putStr "?- "
          hFlush stdout
          inputGoal <- getLine
          case resolve program =<< parse goalParser inputGoal of
            Nothing  -> putStrLn "false."
            Just sub -> print sub
