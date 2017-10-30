{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

--import           Control.Monad   (forM_, mapM_)
--
import           Data.List                     (nub, sort)
--import           Data.Monoid                   (Product, Sum)
--
--import           Numeric.GSL.Permutation       (get, random_permute)
--import           System.Random.Shuffle         (shuffle)
import           Test.QuickCheck               (NonNegative (..), Property, (==>), quickCheckAll)
--
import           Folding                       (joinWith, splitOn)
--import           Monoid           (eitherConcat, maybeConcat)
--import           Monoid                        (eitherConcat)
import           OneLiner                      (contains, highestBit, order3, smartReplicate)
import           PatternMatching               (collectEvery, mergeSort, removeAt)
import           Tree                          (Tree, fromList, toList)
import           Weekday                       (Weekday (..), afterDays, daysToParty, isWeekend, nextDay)

prop_order3 :: Ord a => (a, a, a) -> Bool
prop_order3 = (\(a, b, c) -> a <= b && b <= c) . order3

prop_highestBit :: Int -> Property
prop_highestBit x = x >= 1 ==> (\(v, p) -> v == 2 ^ p && v <= x && 2 * v > x) $ highestBit x

prop_smartReplicate :: (Enum a, Eq a) => [a] -> Bool
prop_smartReplicate xs = propSR xs $ smartReplicate xs
  where
    propSR :: (Enum a, Eq a) => [a] -> [a] -> Bool
    propSR []       []   = True
    propSR []       _    = False
    propSR (x:xs'') ys
        | fromEnum x < 1 = propSR xs'' ys
        | null ys        = False
        | otherwise      = case propSR' x (fromEnum x) ys of
            (True, ys') -> propSR xs'' ys'
            (False, _)  -> False
          where
            propSR' :: (Enum a, Eq a) => a -> Int -> [a] -> (Bool, [a])
            propSR' _ 0 ys' = (True, ys')
            propSR' _ _ []  = (False, [])
            propSR' x' n (y:ys')
                | x' == y   = propSR' x' (n - 1) ys'
                | otherwise = (False, ys')

prop_contains :: forall a. Eq a => a -> [[a]] -> Bool
prop_contains x xss = pContains (contains x xss) xss
  where
    pContains :: Eq a => [[a]] -> [[a]] -> Bool
    pContains []       []        = True
    pContains _        []        = False
    pContains yss      (xs:xss')
        | x `notElem` xs         = pContains yss xss'
    pContains []       _         = False
    pContains (ys:yss) (xs:xss') = ys == xs && pContains yss xss'


prop_removeAt :: Eq a => Int -> [a] -> Bool
prop_removeAt n xs
    | n < 0 || n >= length xs = removeAt n xs == (Nothing, xs)
    | otherwise               = case removeAt n xs of
        (Nothing, _)  -> False
        (Just x,  ys) -> (==) xs $ take n ys ++ (x : drop n ys)

prop_collectEvery :: forall a. Eq a => Int -> [a] -> Bool
prop_collectEvery n xs
    | n <= 0 || n >= length xs + 1 = collectEvery n xs == (xs, [])
    | otherwise                    = case pCollectEvery n ys ys' of
        Nothing   -> False
        Just ys'' -> xs == ys''
      where
        (ys, ys') = collectEvery n xs

pCollectEvery :: Eq a => Int -> [a] -> [a] -> Maybe [a]
pCollectEvery n ys'' []
    | length ys'' > n - 1 = Nothing
    | otherwise           = Just ys''
pCollectEvery n ys'' (e:es)
    | length ys'' < n - 1 = Nothing
    | otherwise           = case flip (pCollectEvery n) es $ drop (n - 1) ys'' of
        Nothing    -> Nothing
        Just ys''' -> Just $ take (n - 1) ys'' ++ (e : ys''')

prop_mergeSort :: Ord a => [a] -> Bool
prop_mergeSort xs = mergeSort xs == sort xs

--propEitherConcat :: forall a b. (Eq a, Eq b, Monoid a, Monoid b) => Int -> [a] -> [b] -> Bool
--propEitherConcat seed ls rs = case length ls + length rs of
--    0 -> eitherConcat ([] :: [Either a b]) == (mempty :: a, mempty :: b)
--    l -> let is = map (get $ random_permute seed l) $ replicate l seed
--             (lsum, rsum) = eitherConcat $ shuffle (map Left ls ++ map Right rs) is
--         in lsum == mconcat ls && rsum == mconcat rs
--
--prop_eitherConcat_listSum :: (Eq a, Eq b, Num b) => Int -> [[a]] -> [Sum b] -> Bool
--prop_eitherConcat_listSum = propEitherConcat
--
--prop_eitherConcat_sumProduct :: (Eq a, Num a, Eq b, Num b) => Int -> [Sum a] -> [Product b] -> Bool
--prop_eitherConcat_sumProduct = propEitherConcat

prop_weekday_nextDay :: Bool
prop_weekday_nextDay = nextDay Monday    == Tuesday
                    && nextDay Tuesday   == Wednesday
                    && nextDay Wednesday == Thursday
                    && nextDay Thursday  == Friday
                    && nextDay Friday    == Saturday
                    && nextDay Saturday  == Sunday
                    && nextDay Sunday    == Monday

prop_weekday_afterDays :: NonNegative Int -> Weekday -> Bool
prop_weekday_afterDays n day = afterDays (getNonNegative n) day == iterate nextDay day !! getNonNegative n

prop_weekday_isWeekend :: Bool
prop_weekday_isWeekend =  isWeekend Saturday && isWeekend Sunday && not (isWeekend Monday
    || isWeekend Tuesday || isWeekend Wednesday || isWeekend Thursday || isWeekend Friday)

prop_weekday_daysToParty :: Weekday -> Bool
prop_weekday_daysToParty day = let n = daysToParty day
                               in n >= 0 && n < 7 && afterDays n day == Friday

prop_tree_foldable :: forall a. Ord a => [a] -> Bool
prop_tree_foldable xs = toList (fromList xs :: Tree a) == nub (sort xs)

prop_splitOn_joinWith :: Eq a => a -> [a] -> Bool
prop_splitOn_joinWith x xs = joinWith x (splitOn x xs) == xs


return []

mapBoth :: (forall t1 t2. [t1] -> [t2]) -> [a1] -> [b1] -> ([a2], [b2])
mapBoth f xs ys = (f xs, f ys)

main :: IO ()
main = do
    res <- $quickCheckAll
    if res then
        putStrLn "All tests are passed!"
    else
        putStrLn "Some test failed!"
--
--    putStrLn ""
--    putStrLn ""
--    putStr "maybeConcat [Just [1,2,3], Nothing, Just [4,5]] = "
--    print  (maybeConcat [Just [1,2,3], Nothing, Just [4,5]] :: [Int])
--    putStr "eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] = "
--    print  (eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] :: (Sum Int, [Int]))


--testStringSum :: [String] -> IO ()
--testStringSum = mapM_ $ \arg -> do
--    putStr $ "stringSum \"" ++ arg ++ "\" = "
--    print  $  stringSum arg
