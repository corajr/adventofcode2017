module Advent where

import Control.Arrow ((&&&))
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (minimumBy, maximumBy)
import Data.Tuple (swap)
import Debug.Trace (traceShowId)
import Text.Parsec
import Text.ParserCombinators.Parsec.Number (int)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Program = Program
  { name :: String
  , weight :: Int
  , holding :: [String]
  } deriving (Eq, Show, Ord)

pName = manyTill anyChar (choice . map (lookAhead . try) $ [space, char ',', newline])

pNameList = do
  first <- pName
  next <- pRemainingNames
  return (first : next)

pRemainingNames = (string ", " >> pNameList) <|> return []

pProgram = do
  name <- pName
  space
  char '('
  weight <- int
  char ')'
  children <- option [] $ try (string " -> ") *> pNameList
  return $ Program name weight children

pPrograms = many (pProgram <* newline) <* eof

getRoot :: [Program] -> String
getRoot xs =
  let hasChildren = Set.fromList (map name $ filter (not . null . holding) xs)
      areChildren = Set.fromList . concatMap holding $ xs
  in Set.elemAt 0 (hasChildren Set.\\ areChildren)

part1 :: String -> String
part1 = either (const "") id . fmap getRoot . parse pPrograms ""

part2 :: String -> Int
part2 = either (const 0) id . fmap f . parse pPrograms ""
  where
    f xs = maybe 0 id . checkWeights $ root
      where
        root = getRoot xs
        programs = Map.fromList (map (name &&& id) xs)
        programWeights = Map.mapWithKey (\k _ -> subtowerWeight k) programs
        checkWeights progName =
          let kids = getChildren progName
              kidsWeights = Map.fromList $ map (\x -> (x, Map.findWithDefault 0 x programWeights)) kids
              reverseKidsWeights = Map.fromList . map swap . Map.toList $ kidsWeights
              weightCounts = Map.fromListWith (+) . map (\x -> (x, 1)) $ Map.elems kidsWeights
              differentWeight = fst . minimumBy (compare `on` snd) $ Map.toList weightCounts
              differentKid = reverseKidsWeights Map.! differentWeight
              correctWeight = fst . maximumBy (compare `on` snd) $ Map.toList weightCounts
              differentKidIndividual = getWeight differentKid
              differentKidKids = getChildren progName
              subtree = if not (null differentKidKids) then checkWeights differentKid else Nothing
              diff = correctWeight - differentWeight
          in  if Map.size weightCounts <= 1  -- all looks balanced, may need to check kids
              then asum (map checkWeights kids)
              else asum [subtree, Just (differentKidIndividual + diff)]
        getChildren progName = maybe [] id (fmap holding $ Map.lookup progName programs)
        getWeight progName = maybe 0 id (fmap weight $ Map.lookup progName programs)
        subtowerWeight progName = maybe 0 id $ do
          childrenNames <- fmap holding $ Map.lookup progName programs
          let ownWeight = getWeight progName
              otherWeights = map subtowerWeight childrenNames
          return $ ownWeight + sum otherWeights

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/07.txt"
  print $ part2 input
