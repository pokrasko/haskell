{-# LANGUAGE TupleSections #-}

module ParserCombinators where

import           Control.Applicative (Alternative(..))
import           Control.Monad       ((>=>))
import           Data.Char           (isDigit, isUpper)

newtype Parser a = Parser
                 { runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f (Parser rp) = Parser $ rp >=> Just . first f
      where
        first :: (a -> b) -> (a, c) -> (b, c)
        first f (x, y) = (f x, y)

instance Applicative Parser where
    pure r = Parser $ Just . (r,)
    (Parser frp) <*> (Parser xrp) = Parser $
        frp    >=> \(f, s')  ->
        xrp s' >>= \(x, s'') ->
        Just (f x, s'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser xrp) <|> (Parser yrp) = Parser $ \s -> xrp s <|> yrp s


boolMaybe :: Bool -> a -> Maybe a
boolMaybe False _ = Nothing
boolMaybe True  x = Just x

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []     = Nothing
    f (x:xs) = boolMaybe (p x) (x, xs)

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs = boolMaybe (not $ null ns) (read ns, rest)
      where
        (ns, rest) = span isDigit xs

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = abParser *> pure ()

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> posInt <* char ' ' <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = posInt *> pure () <|> satisfy isUpper *> pure ()
