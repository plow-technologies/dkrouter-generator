module DirectedKeys.GenerateCfgSpec (main, spec) where

import DirectedKeys.GenerateCfg
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MatchKeys" $ do
    it "Create a list of bounds that should be exact" $ do
      matchKeys ([1..13] :: [Int]) [1..4] `shouldBe` zip [1,5,9,13] [1..4]
    it "Create a list of bounds that should be exactly these bounds" $ do
      matchKeys [1,2,3,4,5,6,7,8,9,10] [1..3] `shouldBe` [(1,1),(5,2),(10,3)]
    it "Create a list of bounds that should be 1 + size of the second list" $ do
      length (matchKeys ([1..10] :: [Int]) [1..4]) `shouldBe` ( length [1..4])
    it "Create a list of bounds that should be  size of the second list" $ do
      length (matchKeys ([1..12382] :: [Int]) [1..12]) `shouldBe` ( length [1..12])
  describe "Bounds Tuples" $ do
    it "Should create bounds from a list pairing up items next to each other" $ do
      createBoundsTuples [1..10] [2..10] `shouldBe` [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]


