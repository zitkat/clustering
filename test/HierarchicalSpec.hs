module HierarchicalSpec where

import Clustering.Hierarchical


import Test.Hspec
import Test.QuickCheck
import GHC.Float.RealFracMethods (roundFloatInt)


spec :: Spec 
spec = do
    describe "Support functions" $ do
            it "SelectMMin" $ do
                selectMMin Nothing (Just (1, 2)) `shouldBe` Just (1, 2)