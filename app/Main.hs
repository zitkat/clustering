module Main where


import Clustering.Sequential
import Clustering.Measures
import Clustering.Data
import Clustering.Hierarchical


main :: IO ()
main = do
    print $ ttsas 2.2 4 meanCDeuc xs
    putStr $ showD  [gasd reprDD meanDRU xs2]


