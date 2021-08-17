module Clustering.Hierarchical where

import Numeric.LinearAlgebra
import Data.List.Lens
import Control.Lens
import Control.Applicative


import Clustering.Util
import Debug.Trace ( trace )

data Dendrogram a = DNode {
                repr :: a,
                level :: Int,
                left :: Dendrogram a,
                right :: Dendrogram a} | DNil

dLeaf :: a -> Dendrogram a
dLeaf a = DNode a 0 DNil DNil

instance (Show a) => Show (Dendrogram a) where
    show d = showD [d]


showD :: Show a => [Dendrogram a] -> String
showD [DNil] = ""
showD ds  = concatMap  ((\s -> "{" ++ s ++ "} ") . show . items) ds
            ++ "\n" ++ 
            if v < 0 then showD ([left ed, right ed] ++ rds)
                     else ""
        where
            (Just (i, v)) = argminval negate $ map level ds
            (rds, Just ed) = pop i ds

items :: Dendrogram a -> [a]
items DNil = []
items DNode{repr=rep, left=DNil, right=DNil} = [rep]
items DNode{repr=rep, left=l, right=r} = items l ++ items r


findClosest2 :: (Ord t) => (a -> a -> t) -> [a] -> Maybe ((Int, Int), t)
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
        findClosest1s' left (x:right) cl = 
            findClosest1s' (left ++ [x]) right $
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

merge ::(Ord t) => t -> ([a] -> [a] -> t) -> [[a]] -> [[a]]
merge _ _ [] = []
merge m f ls = case findClosest2 f ls of
  Nothing          -> ls
  Just ((i, j), v) -> if v < m
                        then merge m f $ merge2 i j ls
                        else ls

merge2 :: Int -> Int -> [[a]] -> [[a]]
merge2 i j ls = deleteList j $ ix i %~ (\l -> l ++ ls !! j) $ ls


agglomerateD :: (Ord t) =>
       (Dendrogram a -> Dendrogram a -> t) -- ^ dendrogram distance
    -> (Dendrogram a -> Dendrogram a -> a) -- ^ representative update
    -> [a] -> Dendrogram a
agglomerateD dd ru xs = agglomerated' 0 $ map dLeaf xs
    where
        agglomerated' _ [] = DNil
        agglomerated' _ ds@(d:_) | length ds == 1 = d
        agglomerated' lev ds = case findClosest2 dd ds of
            Nothing -> error "Nothing to merge!"
            Just ((i, j), v) -> let (newl, newr) = (ds !! i, ds !!j) in
                agglomerated' (lev + 1) $ DNode (ru newl newr) (lev + 1) newl newr : dels [i, j] ds


agglomerate :: (Ord t) => ([a] -> [a] -> t) -> [a] -> [[[a]]]
agglomerate cd xs = agglomerate' [map (:[]) xs]
    where
        agglomerate' [] = []
        agglomerate' [[]] = []
        agglomerate' cs@(c:_) | length c == 1 = cs
        agglomerate' acs@(c:_) = case findClosest2 cd c of
            Nothing -> acs
            Just ((i, j), _) -> agglomerate' $ merge2 i j c:acs


buildSimilarityMtrx :: (a -> a -> Float) -> [a] -> Matrix Float
buildSimilarityMtrx d xs = l >< l $ concatMap (\x -> map (d x) xs) xs
    where
        l = length xs

muas :: Matrix Float -> (Matrix Float -> Int -> Int -> Int) -> [[Int]]
muas p d = undefined -- TODO optimized agglomerative scheme using matrix update


