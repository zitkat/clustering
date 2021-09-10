module Clustering.Stirling where


stirling2 :: (Num p, Ord p) => p -> p -> p
stirling2 n 1 = 1
stirling2 n m | n == m = 1
stirling2 n m | n < m = 0
stirling2 n m = m * stirling2 (n - 1) m + stirling2 (n - 1) (m - 1)


cpurity :: (Fractional r, Eq a, Functor t, Foldable t) => t [a] -> t [a] -> r
cpurity clu cls = 1 / fromIntegral n * fromIntegral (prodintersectN cls clu)
    where n = sum $ fmap length cls

ccoverage :: (Fractional r, Eq a, Functor t, Foldable t) => t [a] -> t [a] -> r
ccoverage = flip cpurity


prodintersectN :: (Foldable t1, Foldable t2, Functor t1, Functor t2, Eq a) => 
    t2 [a] -> t1 [a] -> Int
prodintersectN s1 s2 = sum $ maxintN s1 <$> s2
    where 
        maxintN ss s = maximum $ intersectN s <$> ss 


intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (a:s1) s2 = if a `elem` s2 then a : intersect s1 s2
                                     else intersect s1 s2

intersectN :: Eq a => [a] -> [a] -> Int
intersectN = intersectN' 0
    where 
        intersectN' n [] _ = n
        intersectN' n _ [] = n
        intersectN' n (a:s1) s2 | a `elem` s2 = intersectN' (n + 1) s1 s2 -- TODO would some sort of strict evaluation be helpful here?
                                |otherwise    = intersectN' n s1 s2
