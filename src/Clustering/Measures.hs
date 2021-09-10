module Clustering.Measures where

import           Data.Maybe ( fromMaybe )
import qualified Control.Foldl as L


import qualified Data.Vector as V
import Numeric.LinearAlgebra

import Clustering.Hierarchical
import Clustering.Data

meanCD :: Fractional c => c -> [c] -> c
meanCD a = L.fold L.mean . map (abs . (a -))

maxCD :: (Ord c, Fractional c) => c -> [c] -> c
maxCD a = fromMaybe 0 . L.fold L.maximum . map (abs . (a -))

getPatternMatrix :: [V.Vector Float] -> Matrix Float
getPatternMatrix [] = undefined
getPatternMatrix xs@(x:_) = (length xs >< length x) $  concatMap V.toList xs

xs :: [V.Vector Float]
xs = map V.fromList [[2, 5],
                     [6, 4],
                     [5, 3],
                     [2, 2],
                     [1, 4],
                     [5, 2],
                     [3, 3],
                     [2, 3]]

xs2 :: [V.Vector Float]
xs2 = map V.fromList [[1, 1],
                      [2, 1],
                      [5, 4],
                      [6, 5],
                      [6.5, 6]]


euc :: V.Vector Float -> V.Vector Float -> Float
-- euc v1 v2 = sqrt $ V.sum $ V.zipWith (\a b ->  (a - b)^2) v1 v2
euc = ((sqrt . V.sum) .) . V.zipWith (\a b ->  (a - b)^2)

meanCDeuc :: V.Vector Float -> [V.Vector Float] -> Float
meanCDeuc v l = euc v $ ccentre l

ccentre :: (Fractional b, Foldable t) => t (V.Vector b) -> V.Vector b
ccentre l = V.map (/(fromIntegral $ length l))
            $ foldl1 (V.zipWith (+)) l


meanCCD :: [V.Vector Float] -> [V.Vector Float] -> Float
meanCCD l1 l2 = euc (ccentre l1) (ccentre l2)


meanAgg :: Fractional c => (t -> a -> c) -> t -> [a] -> c
meanAgg f a = L.fold L.mean . map (f a)


reprDD :: Dendrogram (V.Vector Float ) 
    -> Dendrogram (V.Vector Float )  -> Float
reprDD DNil DNil = 0
reprDD DNil _ = 0
reprDD _ DNil = 0
reprDD DNode{repr=r1} DNode{repr=r2} = euc r1 r2

meanDRU :: Dendrogram (V.Vector Float )  
    -> Dendrogram (V.Vector Float)  -> V.Vector Float
meanDRU DNil DNil = undefined
meanDRU DNil _ = undefined
meanDRU _ DNil = undefined
meanDRU DNode{repr=r1} DNode{repr=r2} = ccentre [r1, r2]