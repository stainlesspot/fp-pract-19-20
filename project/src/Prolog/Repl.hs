module Prolog.Repl (repl) where

import Prolog.Program (Program, Term(..))
import Prolog.ProgramParser (goalParser)
import Prolog.Unification (Substitution, Replacement(..))
import Prolog.Goal (resolve)
import Parser (parse)

import Control.Monad (when, forever)
import System.IO
  ( stdin
  , stdout
  , hFlush
  , hSetBuffering
  , BufferMode( NoBuffering, LineBuffering )
  )

-- Displays a list as comma separated values,
-- using a given display function for each value.
displayCSV :: (a -> String) -> [a] -> String
displayCSV _       []     = ""
displayCSV display [x]    = display x
displayCSV display (x:xs) = display x ++ ", " ++ displayCSV display xs

displayTerm :: Term -> String
displayTerm (Var x)     = x
displayTerm (Func f []) = f
displayTerm (Func f ts) = f ++ "(" ++ displayCSV displayTerm ts ++ ")"

displayReplacement :: Replacement -> String
displayReplacement (x := t) = x ++ " := " ++ displayTerm t

displaySubstitution :: Substitution -> String
displaySubstitution = displayCSV displayReplacement


printOneByOne :: (a -> String) -> (Char -> Bool) -> [a] -> IO ()
printOneByOne _ _ [] = pure ()
printOneByOne display _ [x] = putStrLn $ display x
printOneByOne display isContinue (x:xs) = do
  putStrLn $ display x
  c <- getChar
  when (isContinue c) $ printOneByOne display isContinue xs

printSubstitutions :: [Substitution] -> IO ()
printSubstitutions ss = do
  hSetBuffering stdin NoBuffering
  printOneByOne displaySubstitution isContinue ss
  hSetBuffering stdin LineBuffering
  where
    isContinue c = c `elem` "; "

repl :: Program -> IO ()
repl program = forever $ do
  putStr "?- "
  hFlush stdout
  inputGoal <- getLine
  case resolve program <$> parse goalParser inputGoal of
    Nothing   -> putStrLn "Not a valid goal."
    Just []   -> putStrLn "false."
    Just [[]] -> putStrLn "true."
    Just ss   -> printSubstitutions ss
