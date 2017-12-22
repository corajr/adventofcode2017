module Advent where

import Data.List (transpose, unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId)
import Text.Parsec

type Pattern = [String]

data Rule = Rule
  { beforePat :: Pattern
  , afterPat :: Pattern
  } deriving (Eq, Show)

startingPattern :: Pattern
startingPattern =
  [ ".#."
  , "..#"
  , "###"
  ]

type Ruleset = Map Pattern Pattern

makeRuleset :: [Rule] -> Ruleset
makeRuleset = Map.unions . map f
  where
    f (Rule pre post) = let allPre = rotations pre
                        in Map.fromList . map (\x -> (x, post)) $ allPre

applyRules :: Ruleset -> Pattern -> Pattern
applyRules rules xs
  | n `mod` 2 == 0 = joinTiles $ map f $ splitIntoTiles 2 xs
  | n `mod` 3 == 0 = joinTiles $ map f $ splitIntoTiles 3 xs
  | otherwise = error (show xs)
  where
    n = length xs
    f x = maybe (error $ show x) id $ Map.lookup x rules

pPixel = choice [char '.', char '#']
pPattern = many pPixel `sepBy` char '/'
pRule = do
  beforePat <- pPattern
  string " => "
  afterPat <- pPattern
  pure $ Rule beforePat afterPat

pRules = pRule `endBy` newline

rotations :: Pattern -> [Pattern]
rotations x = do
  let xs = [id, transpose, reverse, map reverse]
  f <- xs
  g <- xs
  h <- xs
  pure . f . g . h $ x

chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

concatByRow xs = map (\i -> concatMap (!! i) xs) [0..length xs - 1]

concatByCol :: [[[a]]] -> [[a]]
concatByCol xs@(x:_) = map (\i -> concatMap (!! i) xs) [0..m-1]
  where
    m = length x

arrangeByCol :: [[a]] -> [[a]]
arrangeByCol = transpose

splitIntoTiles :: Int -> Pattern -> [Pattern]
splitIntoTiles n x = concatMap arrangeByCol . map (map (chunks n)) $ chunks n x

joinTiles :: [Pattern] -> Pattern
joinTiles [x] = x
joinTiles xs = concatMap concatByCol . chunks n' $ xs
  where
    n = length xs
    n' = floor (sqrt (fromIntegral n))

part1 :: Int -> String -> Int
part1 n xs = countOn (iterate (applyRules rules) startingPattern !! n)
  where
    rules = makeRuleset $ either (error . show) id $ parse pRules "" xs
    countOn = length . filter (== '#') . concat

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/21.txt"
  print $ part1 5 input
  print $ part1 18 input
