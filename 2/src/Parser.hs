{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import           Control.Applicative (Alternative(..))
import           Control.Monad       ((>=>))
import           Data.Char           (isDigit)
import           Data.List           (foldl')

import           Error               (Error(..), boolEither)


newtype Monstupar s a = Monstupar
                 { runParser :: [s] -> Either Error ([s], a)}

type Parser = Monstupar Char

instance Functor (Monstupar s) where
    fmap f (Monstupar rp) = Monstupar $ rp >=> Right . second f
      where
        second :: (a -> b) -> (c, a) -> (c, b)
        second f (x, y) = (x, f y)

instance Applicative (Monstupar s) where
    pure r = Monstupar $ Right . (, r)

    (Monstupar frp) <*> (Monstupar xrp) = Monstupar $
        frp    >=> \(s', f)  ->
        xrp s' >>= \(s'', x) ->
        Right (s'', f x)

instance Alternative (Either Error) where
    empty = Left ParseError
    (Left _) <|> y = y
    x        <|> _ = x

instance Alternative (Monstupar s) where
    empty = Monstupar $ Left . const ParseError
    (Monstupar xrp) <|> (Monstupar yrp) = Monstupar $ \s -> xrp s <|> yrp s

instance Monad (Monstupar s) where
    xp >>= yf = Monstupar $ runParser xp >=> \(s, x) -> runParser (yf x) s


boolEitherParseError :: Bool -> a -> Either Error a
boolEitherParseError cond = boolEither cond ParseError

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Monstupar f
  where
    f []     = Left ParseError
    f (x:xs) = boolEitherParseError (p x) (xs, x)

char :: Char -> Parser Char
char c = satisfy (== c)

word :: String -> Parser String
word = foldr (\c pcs -> (:) <$> char c <*> pcs) (pure [])

posInt :: Parser Int
posInt = Monstupar f
  where
    f :: String -> Either Error (String, Int)
    f xs = boolEitherParseError (not $ null ns) (rest, read ns)
      where
        (ns, rest) = span isDigit xs

parseEOL :: Parser ()
parseEOL = Monstupar $ \s -> boolEitherParseError (null s) ([], ())
