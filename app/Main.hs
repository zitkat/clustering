module Main where


import Clustering.Sequential
import Clustering.Measures


main :: IO ()
main = do
    print $ ttsas 2.2 4 meanCDeuc xs


