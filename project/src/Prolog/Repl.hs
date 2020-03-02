module Prolog.Repl (repl) where

import Prolog.Program (Program, Term(..))
import Prolog.ProgramParser (goalParser)
import Prolog.Unification (Substitution, Replacement(..))
import Prolog.Goal (resolve)
import Parser (parse)

import Control.Monad (when, unless, forever)
import System.IO
  ( stdin
  , stdout
  , stderr
  , hPutStrLn
  , hFlush
  , hSetBuffering
  , BufferMode (NoBuffering, LineBuffering)
  )
import Data.List (intercalate)

-- Displays a list as comma separated values,
-- using a given display function for each value.
displayCSV :: (a -> String) -> [a] -> String
displayCSV display = intercalate ", " . map display

displayTerm :: Term -> String
displayTerm (Var x)     = x
displayTerm (Func f []) = f
displayTerm (Func f ts) = f ++ "(" ++ displayCSV displayTerm ts ++ ")"

displayReplacement :: Replacement -> String
displayReplacement (x := t) = x ++ " = " ++ displayTerm t

displaySubstitution :: Substitution -> String
displaySubstitution [] = "true."
displaySubstitution s  = displayCSV displayReplacement s

-- Prints a list one element at a time, waiting for the user to input a character.
-- It determines whether to continue printing or not.
printOneByOne :: (a -> String) -> (Char -> Bool) -> [a] -> IO ()
printOneByOne _ _ [] = pure ()
printOneByOne display isContinue (x:xs) = do
  putStrLn $ display x
  c <- getChar
  when (isContinue c) $ printOneByOne display isContinue xs

-- Interactively prints substitutions one by one. They may be skipped altogether. 
printSubstitutions :: [Substitution] -> IO ()
printSubstitutions []   = putStrLn "false."
printSubstitutions ss   = do
  hSetBuffering stdin NoBuffering
  printOneByOne displaySubstitution isContinue ss
  hSetBuffering stdin LineBuffering
  where
    isContinue :: Char -> Bool
    isContinue c = c `elem` "; "

-- Continuously reads a goal from the user
-- and tries to resolve it from the program.
repl :: Program -> IO ()
repl program = forever $ do
  putStr "?- "
  hFlush stdout
  inputGoal <- getLine
  case resolve program <$> parse goalParser inputGoal of
    Nothing   -> unless (inputGoal == "")
                   $ hPutStrLn stderr "Not a valid goal."
    Just ss   -> printSubstitutions ss
