module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec
import Control.Monad (forM_, replicateM)
import qualified Data.Set as Set
-- import Control.Monad.State.Strict (execState, evalState)

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

examplePorts = unlines
  [ "0/2"
  , "2/2"
  , "2/3"
  , "3/4"
  , "3/5"
  , "0/1"
  , "10/1"
  , "9/10"
  ]

examplePortsParsed =
  [ mkPort 0 2
  , mkPort 2 2
  , mkPort 2 3
  , mkPort 3 4
  , mkPort 3 5
  , mkPort 0 1
  , mkPort 1 10
  , mkPort 9 10
  ]

validBridges =
  [ [Port (Closed 0) (Open 1)]
  , [Port (Closed 0) (Open 1), Port (Closed 1) (Open 10)]
  , [Port (Closed 0) (Open 1), Port (Closed 1) (Open 10), Port (Closed 9) (Open 10)]
  , [Port (Closed 0) (Open 2)]
  , [Port (Closed 0) (Open 2)]
  , [Port (Closed 0) (Open 2), Port (Closed 2) (Open 3)]
  , [Port (Closed 0) (Open 2), Port (Closed 2) (Open 3), Port (Closed 3) (Open 5)]
  , [Port (Closed 0) (Open 2), Port (Closed 2) (Open 2)]
  , [Port (Closed 0) (Open 2), Port (Closed 2) (Open 2), Port (Closed 2) (Open 3)]
  , [Port (Closed 0) (Open 2), Port (Closed 2) (Open 2), Port (Closed 2) (Open 3), Port (Closed 3) (Open 4)]
  , [Port (Closed 0) (Open 2), Port (Closed 2) (Open 2), Port (Closed 2) (Open 3), Port (Closed 3) (Open 5)]
  ]


spec :: Spec
spec = do
  describe "canConnect" $ do
    let ex' = ex . canConnect
    ex' (mkPort 0 1) (mkPort 2 3) False
    ex' (mkPort 0 2) (mkPort 0 3) True
    ex' (mkPort 0 2) (mkPort 2 3) True
    ex' (mkPort 0 3) (mkPort 2 3) True
    ex' (mkPort 1 2) (mkPort 0 1) True
  describe "connect" $ do
    let ex' a b out = ex id (connect a b) out
        pOO = Port (Open 0) (Open 1)
        pXO = Port (Closed 0) (Open 1)
        pOX = Port (Open 0) (Closed 1)
        pXX = Port (Closed 0) (Closed 1)
    ex' pOO pXO [(pOX, pXX)]
    ex' pOX pXO []
    ex' pOX pOX [(pXX, pXX)]
    ex' pXX pXO []
  describe "pPorts" $ do
    it "should parse the example" $
      parse pPorts "" examplePorts `shouldBe` Right examplePortsParsed
  describe "remove" $ do
    let start = mkPort 0 2
        maps = portMaps [mkPort 0 2, mkPort 0 1, mkPort 0 3, mkPort 2 3]
        maps' = portMaps [mkPort 0 1, mkPort 0 3, mkPort 2 3]
        ex' maps = ex (\x -> maps `remove` x)
    ex' maps start maps'
  describe "doConnect" $ do
    let start = Port (Closed 0) (Open 2)
        end = Port (Open 2) (Open 3)
        end' = Port (Closed 2) (Open 3)
        maps = portMaps [start, end]
        maps' = portMaps [end']
    it "should connect two ports" $
      doConnect start end maps `shouldBe` [(end, maps')]
  describe "starters" $ do
    let maps = portMaps [mkPort 0 2, mkPort 1 2]
        maps' = portMaps [mkPort 1 2]
    it "should return starting positions" $
      starters maps `shouldBe` [(Port (Closed 0) (Open 2), maps')]
  describe "getPossibleNext" $ do
    let start = Port (Closed 0) (Open 2)
        end = mkPort 2 3
        end' = Port (Closed 2) (Open 3)
        maps = portMaps [start, end]
        ex' = ex (\x -> getPossibleNext x maps)
        nexts = [(end, portMaps [end'])]
    ex' start nexts
  describe "bridgesFrom" $ do
    let maps = portMaps examplePortsParsed
    it "should return the examples" $ do
      let bridges = bridgesFrom (starters maps)
      length bridges `shouldBe` length validBridges
      bridges `shouldBe` validBridges
  describe "part1" $ do
    let ex' = ex part1
    ex' examplePorts 31
