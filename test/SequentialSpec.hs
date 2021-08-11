module SequentialSpec where

import Clustering.Sequential
import Clustering.Measures
import Test.Hspec
import Test.QuickCheck
import GHC.Float.RealFracMethods (roundFloatInt)


spec :: Spec 
spec = do
    describe "Clustering schemas" $ do
            bsasTest "BSAS" bsas
            bsasTest "MBSAS" mbsas
            ttsasTest "TTSAS" ttsas
    describe "Support functions" $ do
        it "SelectMMin" $ do
            selectMMin Nothing (Just (1, 2)) `shouldBe` Just (1, 2)

bsasTest :: String -> (Float -> Int -> (Float -> [Float] -> Float) -> [Float] -> [[Float]]) -> SpecWith ()
bsasTest name sch = describe name $ do
        lengthProp (sch 2.0 10 meanCD)
        

ttsasTest name sch = describe name $ do
        lengthProp (sch 1.0 3.0 meanCD)
        it "example 12.3" $ do
            length (ttsas 2.2 4 meanCDeuc xs) `shouldBe` 2


lengthProp :: Foldable t => ([Float] -> [t a]) -> SpecWith ()
lengthProp sch = it "length" $ do
     property (\(NonNegative n) ->
            sum (map length $ sch [0..n]) == 1 + roundFloatInt n)