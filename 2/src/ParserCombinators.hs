module ParserCombinators where

import           Control.Applicative ((<|>))
import           Data.Char           (isAlpha, isAlphaNum, isSpace, isUpper)

import           Parser              (Parser, satisfy, char, posInt)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = abParser *> pure ()

intPair :: Parser [Int]
intPair = (\x y -> [x, y]) <$> posInt <* char ' ' <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = posInt *> pure () <|> satisfy isUpper *> pure ()
