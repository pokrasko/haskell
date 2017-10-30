module Folding where


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = foldr (f d) [[]]
  where
    f :: Eq a => a -> a -> [[a]] -> [[a]]
    f _  _  []       = error "" -- Never happens
    f e' x  (xs:xss)
        | e' == x    = [] : xs : xss
        | otherwise  = (x:xs) : xss

joinWith :: a -> [[a]] -> [a]
joinWith _ [] = error "Folding.joinWith: Nothing to join"
joinWith d xs = drop 1 $ foldr (\s1 -> (d :) . (s1 ++)) [] xs

