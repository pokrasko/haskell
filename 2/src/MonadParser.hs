module MonadParser where

import           Control.Applicative ((<|>))
import           Control.Monad       ((>=>))
import           Data.List           (foldl')

import           EExpr               (Binding, EExpr(..), Operation(..), eval, precedence)
import           Error               (Error(..))
import           Parser              (Monstupar(..), Parser, char, parseEOL, posInt, word)
import           ParserCombinators   (ident, spaces, zeroOrMore)
import           SParser             (Ident)


type EitherBindings = Either Error [Binding]


parsePrimary :: Parser EExpr
parsePrimary = Const <$> posInt <|> Var <$> ident

parseComb :: Parser EExpr
parseComb = char '(' *> parseEExpr <* char ')'

parsePlusMinus :: Parser Operation
parsePlusMinus = char '+' *> pure Plus  <|>
                 char '-' *> pure Minus

parseAsteriskSlash :: Parser Operation
parseAsteriskSlash = char '*' *> pure Multiply <|>
                     char '/' *> pure Divide

parsePowerSign :: Parser Operation
parsePowerSign = char '^' *> pure Power

parseEExpr :: Parser EExpr
parseEExpr = spaces *> parseSum <* spaces
  where
    parseConst :: Parser EExpr
    parseConst = Const <$> posInt

    parseVar :: Parser EExpr
    parseVar = Var <$> ident

    parseLeftPrecedence :: Parser Operation -> Parser EExpr -> Parser EExpr
    parseLeftPrecedence ps np = np >>= plp' ps np

    plp' :: Parser Operation -> Parser EExpr -> EExpr -> Parser EExpr
    plp' sp np e1 = (spaces *> sp >>= \s -> spaces *> np >>= \e2 -> plp' sp np (App e1 s e2)) <|> pure e1

    parseRightPrecedence :: Parser Operation -> Parser EExpr -> Parser EExpr
    parseRightPrecedence ps np = zeroOrMore ((,) <$> np <* spaces <*> ps <* spaces) >>= \pairs ->
        np >>= \ef ->
        pure $ foldr (\(e1, op) e2 -> App e1 op e2) ef pairs

    parseSum :: Parser EExpr
    parseSum = parseLeftPrecedence parsePlusMinus parseMulDiv

    parseMulDiv :: Parser EExpr
    parseMulDiv = parseLeftPrecedence parseAsteriskSlash parsePower

    parsePower :: Parser EExpr
    parsePower = parseRightPrecedence parsePowerSign parseAtomic

    parseAtomic :: Parser EExpr
    parseAtomic = char '(' *> parseEExpr <* char ')' <|> parseConst <|> parseVar

parseLet :: Parser (Ident, EExpr)
parseLet = (,) <$> (spaces *> word "let" *> spaces *> ident <* spaces <* char '=') <*> parseEExpr <* parseEOL

parseAndFold :: [String] -> EitherBindings
parseAndFold [] = undefined
parseAndFold xs = pac xs

pac :: [String] -> EitherBindings
pac = pac' [] >=> Right . reverse
  where
    pac' :: [Binding] -> [String] -> EitherBindings
    pac' bs []     = Right bs
    pac' bs (s:ss) = runParser parseLet s >>= \(_, (x, e)) ->
        if elem x $ map fst bs
        then Left $ MultipleBindings x
        else eval bs e >>= \i ->
            pac' ((x, i) : bs) ss

processLets :: [String] -> IO ()
processLets ls = case parseAndFold ls of
    Left e   -> print e
    Right bs -> mapM_ putStrLn $ flip map bs $ \(x, v) -> "let " ++ x ++ " = " ++ show v
