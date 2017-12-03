module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (input ++ " ==> " ++ show output) $
    f input `shouldBe` output

spec :: Spec
spec = do
  describe "inverseCaptcha" $ do
    let shouldProduce = ex inverseCaptcha
    "1122" `shouldProduce` 3
    "1111" `shouldProduce` 4
    "1234" `shouldProduce` 0
    "1234" `shouldProduce` 0
    "91212129" `shouldProduce` 9
  describe "inverseCaptcha2" $ do
    let shouldProduce = ex inverseCaptcha2
    "1212" `shouldProduce` 6
    "1221" `shouldProduce` 0
    "123425" `shouldProduce` 4
    "123123" `shouldProduce` 12
    "12131415" `shouldProduce` 4
