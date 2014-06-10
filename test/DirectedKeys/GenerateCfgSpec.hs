module DirectedKeys.GenerateCfgSpec (main, spec) where

-- import DirectedKeys.GenerateCfg
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateNodeRouting" $ do
    it "should return a routing file" $ do
      True `shouldBe` False


