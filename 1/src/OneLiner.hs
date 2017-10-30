module OneLiner where

import           Data.Foldable  (elem)
import           Data.List      (sort)


order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = (\[x', y', z'] -> (x', y', z')) $ sort [x, y, z]

highestBit :: Int -> (Int, Int)
highestBit x
    | x < 1     = error "There is no power of 2 which is less"
    | otherwise = until (\(y, _) -> y > x `div` 2) (\(y, n) -> (2 * y, n + 1)) (1, 0)

smartReplicate :: Enum a => [a] -> [a]
smartReplicate = concatMap (\x -> flip replicate x $ fromEnum x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem
