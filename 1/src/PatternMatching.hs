module PatternMatching where

import           System.Random  (newStdGen, randomRs)


randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []     = (Nothing, [])
removeAt n xs
    | n < 0       = (Nothing, xs)
removeAt 0 (x:xs) = (Just x, xs)
removeAt n (x:xs) = let t = removeAt (n - 1) xs
                    in (fst t, x : snd t)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery 0 xs = (xs, [])
collectEvery x xs = collectEvery' x x xs
  where
    collectEvery' :: Int -> Int -> [a] -> ([a], [a])
    collectEvery' _ _ []       = ([], [])
    collectEvery' 1 n (x':xs') = let t = collectEvery' n n xs'
                                 in (fst t,      x' : snd t)
    collectEvery' i n (x':xs') = let t = collectEvery' (i - 1) n xs'
                                 in (x' : fst t, snd t     )

stringSum :: String -> Int
stringSum = sum . map readNumber . splitSpaceTabNewline
  where
    splitSpaceTabNewline :: String -> [String]
    splitSpaceTabNewline = split isSpaceTabNewline
    split :: (Char -> Bool) -> String -> [String]
    split _ [] = []
    split f s  = case ws of
        []   -> [w]
        _:cs -> w : split f cs
      where
        (w, ws) = break f s
    isSpaceTabNewline :: Char -> Bool
    isSpaceTabNewline = or . flip map [' ', '\t', '\n'] . (==)
    readNumber :: String -> Int
    readNumber [] = 0
    readNumber s = case head s of
        '+' -> case tail s of
            [] -> read s
            ss -> case head ss of
                '-' -> read s
                _   -> read ss
        _ -> read s

mergeSort :: Ord a => [a] -> [a]
mergeSort l = mergeSort' (length l) l
  where
    unite :: Ord a => [a] -> [a] -> [a]
    unite x1 []             = x1
    unite [] x2             = x2
    unite (x1:x1s) (x2:x2s)
        | x1 < x2           = x1 : unite x1s (x2 : x2s)
        | otherwise         = x2 : unite (x1 : x1s) x2s
    mergeSort' :: Ord a => Int -> [a] -> [a]
    mergeSort' 0 _ = []
    mergeSort' 1 x = x
    mergeSort' n x = unite (mergeSort' ns xl) (mergeSort' (n - ns) xr)
      where
        ns = n `quot` 2
        (xl, xr) = splitAt ns x
