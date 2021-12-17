module Clustering.Sequential where

import Control.Monad ( join )
import Debug.Trace ( trace )
import Clustering.Util


-- | Basic Sequential Algorithmic Scheme
--  with element - cluster distance computed as aggregation element-element distance.
bsas1agg :: (Ord t) =>
    t
    -> Int
    -> (a -> a -> t)                    -- ^ element - element distance
    -> ((a -> a -> t) -> a -> [a] -> t) -- ^ element - cluster distance aggregator
    -> [a]
    -> [[a]]
bsas1agg _ _ _ _ [] = []
bsas1agg _ _ _ _ [x] = [[x]]
bsas1agg theta q d cdagg (x : xs) = bsas1' xs [[x]]
    where
        bsas1' (x : xs) cs = bsas1' xs (updateClusters x cs)
        bsas1' [] cs = cs
        updateClusters x cs = 
            case argmin (cdagg d x) cs of
                Just n -> 
                    if cdagg d x (cs !! n) < theta
                        then x `insertInto` n $ cs
                        else if length cs < q
                                then [x] : cs
                                else take n cs ++ [x:(cs !! n)] ++ drop (n + 1) cs
                Nothing -> error "Should never happen!" -- TODO use typesystem to infer?

-- | Basic Sequential Algorithmic Scheme
bsas :: (Ord t) =>
    t                       -- ^ theta maximal distance to cluster
    -> Int                  -- ^ max number of clusters
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

-- | Modified Basic Sequential Algorithmic Scheme
mbsas :: (Ord t) =>
    t
    -> Int
    -> (a -> [a] -> t)      -- ^ element - cluster distance
    -> [a]
    -> [[a]]
mbsas _ _ _ [] = []
mbsas theta q cd (x : xs) = uncurry doClassify $ doCluster [[x]] xs []
    where
        tryAddCluster clstrs x = 
            case argminval (cd x) clstrs of
                Just (n, val) -> 
                    if val > theta && length clstrs < q
                            then ([x] : clstrs, Nothing)
                            else (clstrs, Just x)
                Nothing -> (clstrs, Just x)

        doCluster clstrs [] rest = (clstrs, rest)
        doCluster clstrs (x:xs) rest = 
            case tryAddCluster clstrs x of
                (newclstrs, Nothing) -> doCluster newclstrs xs rest
                (newclstrs, Just x) -> doCluster newclstrs xs (x : rest)

        doClassify clstrs [] = clstrs
        doClassify clstrs xs = foldl (classifyItem cd) clstrs xs


-- | Two-Threshold Sequential Algorithmic Scheme
ttsas :: (Ord t) =>
    t                       -- ^ upper bound on distance to cluster to merge
    -> t                    -- ^ lower bound to create new cluster
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
