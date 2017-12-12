module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse)
import Data.Either (isRight)

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

examplePrograms = unlines
  [ "0 <-> 2"
  , "1 <-> 1"
  , "2 <-> 0, 3, 4"
  , "3 <-> 2, 4"
  , "4 <-> 2, 3, 6"
  , "5 <-> 6"
  , "6 <-> 4, 5"
  ]

spec :: Spec
spec = do
  describe "pProgram" $ do
    let ex' = ex (parse pProgram "")
    ex' "0 <-> 2" (Right (Program (0, [2])))
    ex' "2 <-> 0, 3, 4" (Right (Program (2, [0, 3, 4])))
  describe "pPrograms" $ do
    it "can parse example" $
      parse pPrograms "" examplePrograms `shouldSatisfy` isRight
  describe "part1" $ do
    let ex' = ex part1
    ex' examplePrograms 6
