module Tree
       ( Tree (..)
       , fromList
       , treeEmpty
       , treeFind
       , treeInsert
       , treeSize
       , toList
       ) where

import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup, (<>))


data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

instance Ord a => Semigroup (Tree a) where
    x <> y = fromList $ toList x ++ toList y

instance Ord a => Monoid (Tree a) where
    mempty = Leaf
    mappend = (<>)

instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node l v r) = foldMap f l `mappend` f v `mappend` foldMap f r
    foldr _ z Leaf         = z
    foldr f z (Node l v r) = flip (foldr f) l $ f v $ foldr f z r


treeEmpty :: Tree a -> Bool
treeEmpty Leaf = True
treeEmpty _    = False

treeSize :: Num b => Tree a -> b
treeSize Leaf         = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeFind :: Ord a => a -> Tree a -> Bool
treeFind _ Leaf         = False
treeFind x (Node l v r) = case compare x v of
    LT -> treeFind x l
    EQ -> True
    GT -> treeFind x r

treeInsert :: Ord a => a -> Tree a -> Maybe (Tree a)
treeInsert x Leaf         = Just $ Node Leaf x Leaf
treeInsert x (Node l v r) = case compare x v of
    LT -> case treeInsert x l of
        Nothing -> Nothing
        Just t  -> Just $ Node t v r
    EQ -> Nothing
    GT -> case treeInsert x r of
        Nothing -> Nothing
        Just t  -> Just $ Node l v t

fromList :: Ord a => [a] -> Tree a
fromList = foldr (\x t -> fromMaybe t $ treeInsert x t) Leaf

toList :: Tree a -> [a]
toList = foldr (:) []
