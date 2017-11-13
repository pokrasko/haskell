{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module EExpr where

import           Control.Monad         ((>=>))

import           Error                 (Error(..))
import           SParser               (Ident)


data Operation
    = Plus
    | Minus
    | Multiply
    | Divide
    | Power

data EExpr = Const Int
           | Var Ident
           | App EExpr Operation EExpr

type Binding = (Ident, Int)
type Result = Either Error Int

instance Show Operation where
    show Plus     = "+"
    show Minus    = "-"
    show Multiply = "*"
    show Divide   = "/"
    show Power    = "^"

instance Show EExpr where
    show (Const n)      = show n
    show (Var x)        = x
    show (App e1 op e2) = let e1b = showBracky e1 op False
                              e2b = showBracky e2 op True
                          in e1b ++ " " ++ show op ++ " " ++ e2b
      where
        showBracky :: EExpr -> Operation -> Bool -> String
        showBracky e@(App _ opi _) opo isRight
            | precedence opi <  precedence opo   = bracked $ show e
            | precedence opi == precedence opo   = case opi of
                Power -> if isRight then show e           else bracked $ show e
                _     -> if isRight then bracked $ show e else show e
            | otherwise                          = show e
        showBracky e               _   _         = show e

        bracked :: String -> String
        bracked es = "(" ++ es ++ ")"


applyOperation :: Operation -> (Int -> Int -> Int)
applyOperation Plus = (+)
applyOperation Minus = (-)
applyOperation Multiply = (*)
applyOperation Divide = div
applyOperation Power = (^)

precedence :: Operation -> Int
precedence Plus = 1
precedence Minus = 1
precedence Multiply = 2
precedence Divide = 2
precedence Power = 3

eval :: [Binding] -> EExpr -> Result
eval _  (Const x)   = Right x
eval bs (Var x)     = case map snd $ filter ((==) x . fst) bs of
    []  -> Left $ MissingBinding x
    [i] -> Right i
    _   -> Left $ MultipleBindings x
eval bs (App x f y) = tryEval f (eval bs x) (eval bs y)
  where
    tryEval :: Operation -> Result -> Result -> Result
    tryEval Divide _ (Right 0) = Left ZeroDivision
    tryEval f      x y         = applyOperation f <$> x <*> y
