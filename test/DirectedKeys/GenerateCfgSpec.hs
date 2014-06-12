module DirectedKeys.GenerateCfgSpec (main, spec) where

import DirectedKeys.GenerateCfg
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MatchKeys" $ do
    it "Create a list of bounds that should be exactly these bounds" $ do
      matchKeys [1..13] [1..4] `shouldBe` [1,4,7,10,13]
    it "Create a list of bounds that should be 1 + size of the second list" $ do
      length (matchKeys [1..5] [1..4]) `shouldBe` (1 + length [1..4])
    it "Create a list of bounds that should be 1 + size of the second list" $ do
      length (matchKeys [1..12382] [1..12]) `shouldBe` (1 + length [1..12])
  describe "Bounds Tuples" $ do
    it "Should create bounds from a list pairing up items next to each other" $ do
      createBoundsTuples [1..10] [2..10] `shouldBe` [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]


