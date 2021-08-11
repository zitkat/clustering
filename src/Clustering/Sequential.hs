module Clustering.Sequential where


import Control.Monad ( join )
import Data.List (elemIndex, foldl1', delete)
import Data.List.Lens
import Control.Lens
import Control.Applicative
import Data.Maybe
import Debug.Trace ( trace )


bsas1agg :: (Ord t) =>
    t
    -> Int
    -> (a -> a -> t)                    -- ^ element - element distance
    -> ((a -> a -> t) -> a -> [a] -> t) -- ^ element - cluster distance aggregator
    -> [a]
    -> [[a]]
bsas1agg _ _ _ _ [] = []
bsas1agg _ _ _ _ [x] = [[x]]
bsas1agg theta q d cd (x : xs) = bsas1' xs [[x]]
    where
        bsas1' (a : as) cs = let newcs = case argmin (cd d a) cs of
                                            Just n -> if cd d a (cs !! n) < theta
                                                then take n cs ++ [a:(cs !! n)] ++ drop (n + 1) cs
                                                else if length cs < q
                                                        then [a] : cs
                                                        else take n cs ++ [a:(cs !! n)] ++ drop (n + 1) cs
                                            Nothing -> error "Should never happen!" -- TODO use typesystem to infer?
                             in bsas1' as newcs
        bsas1' [] cs = cs

bsas :: (Ord t) =>
    t                       -- ^ theta maximal distance to cluster
    -> Int
    -> (a -> [a] -> t)      -- ^ element - cluster distance
    -> [a]
    -> [[a]]
bsas _ _ _ [] = []
bsas theta q cd xs = foldl updateClusters [] xs
    where
        updateClusters clstrs x = case argminval (cd x) clstrs of
            Just (n, val) -> if val < theta || length clstrs > q
                        then x `insertInto` n $ clstrs
                        else [x] : clstrs
            Nothing -> [x] : clstrs

mbsas :: (Ord t) =>
    t
    -> Int
    -> (a -> [a] -> t)      -- ^ element - cluster distance
    -> [a]
    -> [[a]]
mbsas _ _ _ [] = []
mbsas theta q cd (x : xs) = uncurry doClassify $ doCluster [[x]] xs []
    where
        addCl = addClusters theta q cd
        doCluster clstrs [] rest = (clstrs, rest)
        doCluster clstrs (x:xs) rest = case addCl clstrs x of
            (newclstrs, Nothing) -> doCluster newclstrs xs rest
            (newclstrs, Just x) -> doCluster newclstrs xs (x : rest)

        doClassify clstrs [] = clstrs
        doClassify clstrs xs = foldl (classifyItem cd) clstrs xs

ttsas :: (Ord t) =>
    t
    -> t
    -> (a -> [a] -> t)      -- ^ element - cluster distance
    -> [a]
    -> [[a]]
ttsas theta1 theta2 cd = buildClusters 0 [] []
    where
        -- buildClusters n cls left xs | trace ("buildClusters: n=" 
        --                                      ++ show n ++ " cls=" 
        --                                      ++ show cls ++ " left=" 
        --                                      ++ show left ++ " xs="
        --                                      ++ show xs) False = undefined
        buildClusters _ cls [] [] = cls
        buildClusters 0 cls (l:left) [] = buildClusters 0 ([l] : cls) [] left
        buildClusters n cls (l:left) [] = buildClusters 0 cls [] (l:left)
        buildClusters n cls left (x:xs) =
            case argminval (cd x) cls of
                Nothing -> buildClusters (n + 1) ([x] : cls) left xs
                Just (i, val) -> case val of
                    val | val < theta1 -> buildClusters (n + 1) (x `insertInto` i $ cls) left xs
                    val | val > theta2 -> buildClusters (n + 1) ([x] : cls) left xs
                    _ -> buildClusters n cls (x:left) xs

addClusters :: Ord a1 =>
    a1
    -> Int
    -> (a2 -> [a2] -> a1)
    -> [[a2]]
    -> a2
    -> ([[a2]], Maybe a2)
addClusters theta q cd clstrs x = case argminval (cd x) clstrs of
            Just (n, val) -> if val > theta && length clstrs < q
                        then ([x] : clstrs, Nothing)
                        else (clstrs, Just x)
            Nothing -> (clstrs, Just x)

classifyItem :: Ord a1 =>
    (a2 -> [a2] -> a1)
    -> [[a2]]
    -> a2
    -> [[a2]]
classifyItem cd clstrs x = case argmin (cd x) clstrs of
            Just n -> x `insertInto` n $ clstrs
            Nothing -> [x] : clstrs


insertInto :: a -> Int -> [[a]] -> [[a]]
insertInto x n cs = take n cs ++ [x:(cs !! n)] ++ drop (n + 1) cs



merge ::(Ord t) => t -> ([a] -> [a] -> t) -> [[a]] -> [[a]]
merge _ _ [] = []
merge m f ls = case findClosest2 f ls of
  Nothing          -> ls
  Just ((i, j), v) -> if v < m 
                        then merge m f $ deleteList j $ ix i %~ (\l -> l ++ ls !! j) $ ls
                        else ls


findClosest2 :: (Ord t) => ([a] -> [a] -> t) -> [[a]] -> Maybe ((Int, Int), t)
findClosest2 _ []  = Nothing
findClosest2 f cls = getidx r1 r2
    where
        r1 = findClosest1s f cls
        r2 = argminval (snd <$>) r1
        getidx :: [Maybe (Int, t)] -> Maybe (Int, Maybe t) -> Maybe ((Int, Int), t)
        getidx l (Just (i, Just v1)) = case l !! i of
          Nothing -> Nothing
          Just (j, v2) -> Just ((i, j), v1)
        getidx l (Just (i, Nothing)) = Nothing
        getidx _ Nothing = Nothing


findClosest1s :: (Ord t) => (a -> a -> t) -> [a] -> [Maybe (Int, t)]
findClosest1s _ [] = []
findClosest1s f ls = reverse $ findClosest1s' [] ls []
    where
        findClosest1s' left [] cl = cl
        findClosest1s' left (x:right) cl = findClosest1s' (left ++ [x]) right $
                                                 selectMMin  
                                                    (findClosest x f left) 
                                                    ((_1 +~ 1 + length left) <$> findClosest x f right)
                                                    -- shift right index
                                                 : cl

findClosest :: (Ord t) => a -> (a -> a -> t) ->  [a] -> Maybe (Int, t)
findClosest _ _ [] = Nothing
findClosest x d ls = argminval (d x) ls

selectMMin :: Ord b => Maybe (a, b) -> Maybe (a, b) -> Maybe (a, b)
selectMMin (Just (n, v1)) (Just (m, v2)) = if v2 < v1 then Just (m, v2)
                                                      else Just (n, v1)
selectMMin m1 m2 = m1 <|> m2





-- | Taken from
--  https://stackoverflow.com/a/49681662/4749381
deleteN
  :: (Foldable f, Monoid (f a))
  => (a -> f a -> f a) -- ^ cons operator
  -> Int -- ^ index to delete
  -> f a -- ^ initial structure
  -> f a -- ^ resultant structure
deleteN cons n xs = flipTfo xs $ folded . ifiltered (\i _ -> i /= n)
  where
    flipTfo = flip toFoldableOf
    toFoldableOf l = foldrOf l cons mempty

deleteList :: Int -> [a] -> [a]
deleteList = deleteN (:)

argmin :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe Int
argmin f s = let fs = map f s in
    elemIndex (foldl1' min fs) fs

argminval :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe (Int, a1)
argminval f s = let fs = map f s
                    minval = foldl1' min fs
                in
                elemIndex minval fs >>= (\a -> Just (a, minval))

fstf :: (a -> b -> (a, c)) -> a -> b -> a
fstf f a b = fst $ f a b