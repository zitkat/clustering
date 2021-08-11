module Main where


import qualified Spec
import Test.Hspec.Runner ( hspec )



main :: IO ()
main = do
  hspec Spec.spec