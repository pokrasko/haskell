module Nondeterministic where

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \b -> [0 : b, 1 : b]

combinations :: Int -> Int -> [[Int]]
combinations n k
    | n < 0 || k < 0 || k > n = [[]]
    | otherwise               = comb 1 n k
      where
        comb :: Int -> Int -> Int -> [[Int]]
        comb _ _ 0           = [[]]
        comb a b k
            | b - a == k - 1 = [[a .. b]]
            | otherwise      = (comb (a + 1) b (k - 1) >>= \w -> [a : w]) ++ comb (a + 1) b k

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = p' [] xs >>= \(x, xs') -> permutations xs' >>= \xs'' -> [x : xs'']
  where
    p' :: [a] -> [a] -> [(a, [a])]
    p' _  []     = []
    p' ls (x:rs) = (x, ls ++ rs) : p' (x : ls) rs
