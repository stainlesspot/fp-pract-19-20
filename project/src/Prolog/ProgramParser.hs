-- TODO: support whitespace
module Prolog.ProgramParser where

import Data.Char (isUpper, isLower, isAlphaNum, isSpace)

import Data.List.NonEmpty (NonEmpty(..), toList)

import Prolog.DataTypes
  ( Term(..)
  , Atom(..)
  , HornClause(..)
  , UName
  , LName
  , Program
  )

import Parser
  ( Parser
  , parse
  , nom
  , result
  , empty
  , (<|>)
  , many
  , some
  , endOfInput
  )

-- Always parses the given character
char :: Char -> Parser Char
char c = do
  x <- nom
  if x == c
  then result x
  else empty

-- Parse the given string, and only the given string
string :: String -> Parser String
string []     = result []
string (c:cs) = do
  r  <- char c
  rs <- string cs
  result $ r:rs

-- Parse a character that satisfies a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- nom
  if p x
  then result x
  else empty

-- Surround a parser with an opening and closing one
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close inside = do
  open
  r <- inside
  close
  result r

---- A parser that runs both parsers, but ignores the result of the right one.
---- This is traditionally (<*), and is available for all Applicatives
--ignoreRight :: Parser a -> Parser b -> Parser a
--ignoreRight px py = do
--  x <- px
--  py
--  result x

-- Parse a value one or more times, separated by another parser.
sepBy1 :: Parser sep -> Parser a -> Parser (NonEmpty a)
sepBy1 sep p = do
  --xs <- many $ ignoreRight p sep
  xs <- many $ p <* sep
  x  <- p
  result $ case xs of
             []     -> x :| xs
             (y:ys) -> y :| ys ++ [x]


-- Parse many values, each followed by the separator
-- e.g. parse (followedBy (char ',') nom) "a,b,c," == "abc"
--      parse (followedBy (char ',') nom) "a,b,c,d" == "abc"
followedBy :: Parser sep -> Parser a -> Parser [a]
followedBy sep p = many $ p <* sep


-- Parse a nonempty list of values,
-- separated by ',' and enclosed by parenthesis,
-- e.g., parse (vector nom) "(a,b,c,d)" == 'a':|"bcd"
vector1 :: Parser a -> Parser (NonEmpty a)
vector1
  = between (char '(') (char ')')
  . sepBy1 (char ',')


isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c
  = c == '_' || isAlphaNum c

--------------------------------------------------
-----      Parsers for program elements      -----

uNameParser :: Parser UName
uNameParser = do
  u <- satisfy isUpper
  rest <- many $ satisfy isAlphaNumUnderscore
  return $ u:rest

lNameParser :: Parser LName
lNameParser = do
  l <- satisfy isLower
  rest <- many $ satisfy isAlphaNumUnderscore
  return $ l:rest

varParser :: Parser Term
varParser = Var <$> uNameParser

constParser :: Parser Term
constParser = Const <$> lNameParser

funcParser :: Parser Term
funcParser = Func <$> lNameParser <*> vector1 termParser

termParser :: Parser Term
termParser = funcParser <|> constParser <|> varParser

atomParser :: Parser Atom
atomParser = Atom <$> lNameParser <*> vector1 termParser

-- All rules and facts end with '.'
endOfClause :: Parser ()
endOfClause = char '.' *> result ()

factParser :: Parser HornClause
factParser = (:- []) <$> atomParser <* endOfClause

ruleParser :: Parser HornClause
ruleParser = do
  a <- atomParser
  string ":-"
  ps <- sepBy1 (char ',') atomParser
  endOfClause
  return $ a :- toList ps

hornClauseParser :: Parser HornClause
hornClauseParser = factParser <|> ruleParser

optWhitespace :: Parser String
optWhitespace = many $ satisfy isSpace

programParser :: Parser Program
programParser = followedBy optWhitespace hornClauseParser <* endOfInput

goalParser :: Parser [Atom]
goalParser = toList <$> sepBy1 (char ',') atomParser
