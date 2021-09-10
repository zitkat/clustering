module Clustering.Util where

import Data.List (elemIndex, foldl1', delete)
import Data.List.Lens
import Control.Lens


-- argmin :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe Int
-- argmin f s = let fs = map f s in
--     elemIndex (foldl1' min fs) fs

argmin :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe Int
argmin = arg___ min

argminval :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe (Int, a1)
argminval = arg___val min

argmax :: Ord t => (a -> t) -> [a] -> Maybe Int
argmax = arg___ max

argmaxval :: Ord t => (a -> t) -> [a] -> Maybe (Int, t) 
argmaxval = arg___val max

arg___val :: Ord t => (t -> t -> t) -> (a -> t) -> [a] -> Maybe (Int, t)
arg___val sel f s = let fs = map f s
                        val = foldl1' sel fs
                    in elemIndex val fs >>= (\a -> Just (a, val))


arg___ :: (Ord t) => (t -> t -> t) -> (a -> t) -> [a] -> Maybe Int 
arg___ sel f s = let fs = map f s in
    elemIndex (foldl1' sel fs) fs


avg :: (Fractional t, Num t) => [t] -> t
avg xs = (((1 / fromIntegral (length xs)) * ) . sum)  xs

fstf :: (a -> b -> (c, d)) -> a -> b -> c
fstf f a b = fst $ f a b

sndf :: (a -> b -> (c, d)) -> a -> b -> d
sndf f a b = snd $ f a b

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

dels :: [Int] -> [a] -> [a]
dels idcs l = l ^.. folded . ifiltered (\k _ -> foldr (\i b -> i /= k && b) True idcs)

del :: Int -> [a] -> [a]
del idc = dels [idc]

pop :: Int -> [a] -> ([a], Maybe a)
pop i l = (dels [i] l, l ^? ix i)

pops :: [Int] -> [a] -> ([a], [Maybe a])
pops idcs l = (dels idcs l, map (\i -> l ^? ix i) idcs)

classifyItem :: Ord a1 =>
    (a2 -> [a2] -> a1)
    -> [[a2]]
    -> a2
    -> [[a2]]
classifyItem cd clstrs x = 
    case argmin (cd x) clstrs of
            Just n -> x `insertInto` n $ clstrs
            Nothing -> [x] : clstrs


insertInto :: a -> Int -> [[a]] -> [[a]]
insertInto x n cs = take n cs ++ [x:(cs !! n)] ++ drop (n + 1) cs