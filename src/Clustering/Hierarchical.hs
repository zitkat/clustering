{-# LANGUAGE ScopedTypeVariables #-}

module Clustering.Hierarchical where

import Numeric.LinearAlgebra
import Data.List.Lens
import Control.Lens
import Control.Applicative
import Debug.Trace ( trace )

import Clustering.Util
import Clustering.Data

-- | In list of elements find closest pair
findClosest2 :: (Ord t) => 
        (a -> a -> t)               -- ^ element-element distance
        -> [a]                      -- ^ elements
        -> Maybe ((Int, Int), t)    -- (el1. idex, el2 index), 
findClosest2 _ []  = Nothing
findClosest2 f cls = getidx r1 r2
    where
        r1 = findClosest1s f cls
        r2 = argminval (snd <$>) r1
        getidx :: [Maybe (Int, t)] 
            -> Maybe (Int, Maybe t) 
            -> Maybe ((Int, Int), t)
        getidx l (Just (i, Just v1)) = case l !! i of
          Nothing -> Nothing
          Just (j, v2) -> Just ((i, j), v1)
        getidx l (Just (i, Nothing)) = Nothing
        getidx _ Nothing = Nothing


findClosest1s :: (Ord t) => 
        (a -> a -> t)           -- ^ element-element distance
        -> [a]                  -- ^ elements
        -> [Maybe (Int, t)]     -- ^ for each element its closest neighbour index and distance
findClosest1s _ [] = []
findClosest1s f ls = reverse $ findClosest1s' [] ls []
    where
        findClosest1s' left [] cl = cl
        findClosest1s' left (x:right) cl =
            findClosest1s' (left ++ [x]) right $
                        selectMMin
                            (findClosest f x left)
                            ((_1 +~ 1 + length left) <$> findClosest f x right)
                            -- shift right index
                        : cl

findClosest :: (Ord t) => forall a. (a -> a -> t) -> a -> [a] -> Maybe (Int, t)
findClosest _ _ [] = Nothing
findClosest d x ls = argminval (d x) ls

selectMMin :: Ord b => Maybe (a, b) -> Maybe (a, b) -> Maybe (a, b)
selectMMin (Just (n, v1)) (Just (m, v2)) | v2 < v1 = Just (m, v2)
selectMMin (Just (n, v1)) (Just (m, v2)) | v2 > v1 = Just (n, v1)
selectMMin m1 m2 = m1 <|> m2

merge ::(Ord t) => t -> ([a] -> [a] -> t) -> [[a]] -> [[a]]
merge _ _ [] = []
merge m f ls = case findClosest2 f ls of
  Nothing          -> ls
  Just ((i, j), v) -> if v < m
                        then merge m f $ merge2 i j ls
                        else ls

merge2 :: Int -> Int -> [[a]] -> [[a]]
merge2 i j ls = deleteList j $ ix i %~ (\l -> l ++ ls !! j) $ ls

-- | Generalized aggregative scheme using Dendrogram
gasd :: forall a.
    (Dendrogram a -> Dendrogram a -> Float) -- ^ dendrogram-dendrogram distance
    -> (Dendrogram a -> Dendrogram a -> a)  -- ^ representative update
    -> [a]                                  -- ^ elements
    -> Dendrogram a
gasd ddist ru xs = agglomerated' 0 $ map dLeaf xs
    where
        agglomerated' :: Int    -- ^ current level
            -> [Dendrogram a]   -- ^ current disconnected dendrograms
            -> Dendrogram a     -- ^ final dendrogram
        agglomerated' _ [] = DNil
        agglomerated' _ ds@(d:_) | length ds == 1 = d
        agglomerated' level ds = case findClosest2 ddist ds of
            Nothing -> error "Nothing to merge!"
            Just ((i, j), v) -> let (newl, newr) = (ds !! i, ds !! j) in
                agglomerated' (level + 1) $ DNode (ru newl newr)  -- create new dendrogram leaf
                                                  (level + 1) 
                                                  v 
                                                  newl newr 
                                            : dels [i, j] ds      -- add it to the list, deleting its constituents

-- | Generalized aggregative scheme
gas :: forall a t. Ord t =>
    ([a] -> [a] -> t)                   -- ^ cluster-cluster distance
    -> [a]                              -- ^ elements
    -> [[[a]]]                          -- ^ clusterings hierarchy i.e. dendrogram
gas cd xs = agglomerate' [map (:[]) xs]
    where
        agglomerate' :: [[[a]]] -> [[[a]]]
        agglomerate' [] = []
        agglomerate' [[]] = []
        agglomerate' acs@(c:_) | length c == 1 = acs
        agglomerate' acs@(c:_) = case findClosest2 cd c of
            Nothing -> acs
            Just ((i, j), _) -> agglomerate' $ merge2 i j c:acs

-- | Does not presume similarity symetry and hence is full O(n^2)
buildSimilarityMtrx :: (a -> a -> Float) -> [a] -> Matrix Float
buildSimilarityMtrx d xs = l >< l $ concatMap (\x -> map (d x) xs) xs
    where
        l = length xs

-- | Matrix update aggregative scheme
muas :: Matrix Float -> (Matrix Float -> Int -> Int -> Int) -> [[Int]]
muas p d = undefined -- TODO optimized agglomerative scheme using matrix update

-- | Modified general divisive scheme
mgds :: (Ord t, Num t, Fractional t) =>
    (a -> a -> t)       -- ^ element-element dissimiliarity measure
    -> [a]              -- ^ elements
    -> [[[a]]]          -- ^ sequence of clusterings i.e. dendrogram
mgds d [] = []
mgds d xs = divideAll [[xs]]
    where
        divideAll [] = []
        divideAll acs@(c:cs) = if length cs == length xs then acs
                                                         else divideAll $ (c >>= splitCluster) : acs
        splitCluster [] = []
        splitCluster xs = reassign $ outsider xs
                      -- map (((1/fromIntegral (length xs) * ) . sum) . (\x -> map (d x) xs)) xs
        outsider xs = case argmax id $ map (avg . flip map xs . d) xs of
                        Nothing -> error "What happend?!"
                        Just n -> case pop n xs of
                             (as, Nothing) -> ([], as)
                             (as, Just a) -> ([a], as)

        reassign ([], os) = [os]
        reassign (ns, []) = [ns]
        reassign (ns, as) = [ns, as]
                            