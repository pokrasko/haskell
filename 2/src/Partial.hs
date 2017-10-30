{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Partial where

import           Control.Applicative   ((<|>))
import qualified Control.Category as C (Category(..))
import           Control.Monad         ((>=>))
import           Data.Maybe            (fromMaybe, isJust)


data a ~> b
    = Partial   (a -> Maybe b)
    | Defaulted (a ~> b) b

instance C.Category (~>) where
    id = Partial Just
    (Partial fp)      . (Partial gp)      = Partial $ gp >=> fp
    (Partial fp)      . (Defaulted gp gd) = Partial $ fp . fromDefaulted gp gd
    (Defaulted fp fd) . g                 = let ifp = innerPartial fp
                                            in flip Defaulted fd $ ifp C.. innerPartial g


partial :: (a -> Maybe b) -> (a ~> b)
partial = Partial

innerDefaulted :: (a ~> b) -> b -> (a ~> b)
innerDefaulted p@Partial{} d = Defaulted p d
innerDefaulted (Defaulted fp fd) _ = innerDefaulted fp fd

innerPartial :: (a ~> b) -> (a ~> b)
innerPartial p@Partial{} = p
innerPartial (Defaulted fp _) = innerPartial fp

fromDefaulted :: (a ~> b) -> b -> (a -> b)
fromDefaulted p d = fromMaybe d . fp
  where
    Partial fp = innerPartial p

total :: (a -> b) -> (a ~> b)
total = Partial . (Just .)

apply :: (a ~> b) -> a -> Maybe b
apply (Partial fm) x = fm x
apply (Defaulted fp d) x = Just $ applyOrElse fp x d

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse pf x d = fromMaybe d $ apply pf x

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault pf@Partial{} d = Defaulted pf d
withDefault (Defaulted pf od) nd = Defaulted pf nd

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt pf = isJust . apply pf

orElse :: forall a b. (a ~> b) -> (a ~> b) -> (a ~> b)
orElse pf1@Defaulted{} _ = pf1
orElse pf1@Partial{} (Defaulted fp d) = Defaulted (orElse pf1 fp) d
orElse pf1 pf2 = Partial $ \x -> apply pf1 x <|> apply pf2 x
