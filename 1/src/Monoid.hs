module Monoid where

import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup, (<>))


maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = fromMaybe [] . mconcat
-- = fold . fold

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = flip foldr (mempty, mempty) $ \e (ls, rs) -> case e of
    Left l  -> (l `mappend` ls, rs            )
    Right r -> (ls,             r `mappend` rs)


data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a) where
    ix <> iy = Identity $ runIdentity ix <> runIdentity iy

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    ix `mappend` iy = Identity $ runIdentity ix `mappend` runIdentity iy

data Name = NotAName
             | Name String

instance Semigroup Name where
    NotAName <> x        = x
    x        <> NotAName = x
    Name x   <> Name y   = Name $ x ++ ('.' : y)

instance Monoid Name where
    mempty = NotAName
    mappend = (<>)

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    f <> g = Endo $ getEndo f . getEndo g

instance Monoid (Endo a) where
    mempty = Endo id
    mappend = (<>)

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    f <> g = Arrow $ \x -> getArrow f x <> getArrow g x

instance Monoid b => Monoid (Arrow a b) where
    mempty = Arrow $ const mempty
    f `mappend` g = Arrow $ \x -> getArrow f x `mappend` getArrow g x
