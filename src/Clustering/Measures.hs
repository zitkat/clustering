module Clustering.Measures where

import           Data.Maybe ( fromMaybe )
import qualified Control.Foldl as L


import qualified Data.Vector as V

meanCD :: Fractional c => c -> [c] -> c
meanCD a = L.fold L.mean . map (abs . (a -))

maxCD :: (Ord c, Fractional c) => c -> [c] -> c
maxCD a = fromMaybe 0 . L.fold L.maximum . map (abs . (a -))

xs :: [V.Vector Float]
xs = map V.fromList [[2, 5],
                     [6, 4],
                     [5, 3],
                     [2, 2],
                     [1, 4],
                     [5, 2],
                     [3, 3],
                     [2, 3]]

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