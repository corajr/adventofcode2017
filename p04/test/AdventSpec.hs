module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

spec :: Spec
spec = do
  describe "isValidPassphrase" $ do
    let ex' = ex isValidPassphrase
    "aa bb cc dd ee" `ex'` True
    "aa bb cc dd aa" `ex'` False
    "aa bb cc dd aaa" `ex'` True
