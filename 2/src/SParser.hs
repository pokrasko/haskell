module SParser where

import           Control.Applicative ((<|>))

import           Parser            (Parser, char, posInt)
import           ParserCombinators (ident, oneOrMore, spaces)

type Ident = String

data Atom = N Int
          | I Ident
            deriving Show

data SExpr = A Atom
           | Comb [SExpr]
             deriving Show


parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseA <|> parseComb) <* spaces
  where
    parseA = A <$> parseAtom
    parseComb = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')
