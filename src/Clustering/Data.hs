module Clustering.Data where

import Clustering.Util


data Dendrogram a = DNode {
                repr :: a,
                level :: Int,
                meas :: Float,
                left :: Dendrogram a,
                right :: Dendrogram a} | DNil

dLeaf :: a -> Dendrogram a
dLeaf a = DNode a 0 0 DNil DNil

instance (Show a) => Show (Dendrogram a) where
    show d = showD [d]


showD :: (Show a) => [Dendrogram a] -> String
showD [] = ""
showD [DNil] = ""
showD ds = show (negate l) ++ ") " ++ "n=" ++ show (length ds)++ " v=" ++ show (meas ed) ++ ": " 
            ++ concatMap  ((\s -> "{" ++ s ++ "} ") . show . items) ds
            ++ "\n" ++ 
            if l /= 0 then showD ([left ed, right ed] ++ rds)
                      else ""
        where
            (Just (i, l)) = argminval negate $ map level ds
            (rds, Just ed) = pop i ds

items :: Dendrogram a -> [a]
items DNil = []
items DNode{repr=rep, left=DNil, right=DNil} = [rep]
items DNode{repr=rep, left=l, right=r} = items l ++ items r
