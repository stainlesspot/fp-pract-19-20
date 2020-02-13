module Prolog.ProgramParser
  ( upperNameParser
  , lowerNameParser
  , termParser
  , atomParser
  , hornClauseParser
  , programParser
  , goalParser
  ) where

import Data.Char (isUpper, isLower, isAlphaNum, isSpace)

import Data.List.NonEmpty (NonEmpty(..), toList)
import Prolog.Program
  ( UpperName
  , LowerName
  , Term(..)
  , Atom(..)
  , HornClause(..)
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
between open close inside
  = open *> inside <* close

-- Parse a value one or more times, separated by another parser.
sepBy1 :: Parser sep -> Parser a -> Parser (NonEmpty a)
sepBy1 sep p = do
  xs <- many $ p <* sep
  x  <- p
  result $ case xs of
             []     -> x :| xs
             (y:ys) -> y :| ys ++ [x]

-- Parse a nonempty list of values,
-- separated by ',' and enclosed by parenthesis,
-- e.g., parse (vector nom) "(a,b,c,d)" == 'a':|"bcd"
tuple1 :: Parser a -> Parser (NonEmpty a)
tuple1
  = between (char '(' <* optionalWhitespace) (optionalWhitespace *> char ')')
  . commaSeparated

surroundWith :: Parser surr -> Parser a -> Parser a
surroundWith surr = between surr surr

optionalWhitespace :: Parser String
optionalWhitespace = many $ satisfy isSpace

-- parser which allows whitespace around another parser.
spaced :: Parser a -> Parser a
spaced = surroundWith optionalWhitespace

commaSeparated :: Parser a -> Parser (NonEmpty a)
commaSeparated = sepBy1 $ spaced $ char ','


isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c
  = c == '_' || isAlphaNum c

toListParser :: Parser (NonEmpty a) -> Parser [a]
toListParser p = fmap toList p <|> pure []

-- Extends tuple1 to match the empty string.
-- This parser does not match "()".
optionalTuple :: Parser a -> Parser [a]
optionalTuple = toListParser . tuple1

--------------------------------------------------
-----      Parsers for Program elements      -----

upperNameParser :: Parser UpperName
upperNameParser = do
  u <- satisfy isUpper
  rest <- many $ satisfy isAlphaNumUnderscore
  pure $ u:rest

lowerNameParser :: Parser LowerName
lowerNameParser = do
  l <- satisfy isLower
  rest <- many $ satisfy isAlphaNumUnderscore
  pure $ l:rest

varParser :: Parser Term
varParser = Var <$> upperNameParser

funcParser :: Parser Term
funcParser = Func <$> lowerNameParser <*> optionalTuple termParser

termParser :: Parser Term
termParser = varParser <|> funcParser

atomParser :: Parser Atom
atomParser = Atom <$> lowerNameParser <*> tuple1 termParser

hornClauseParser :: Parser HornClause
hornClauseParser = do
  a <- atomParser
  as <- bodyParser
  spaced $ char '.'
  pure $ a :- as
  where bodyParser :: Parser [Atom]
        bodyParser = toListParser $ do
          spaced $ string ":-"
          commaSeparated atomParser

programParser :: Parser Program
programParser
  = between optionalWhitespace endOfInput
  $ many hornClauseParser

goalParser :: Parser [Atom]
goalParser
  = between optionalWhitespace endOfInput
  $ toList <$> commaSeparated atomParser <* spaced (char '.')

