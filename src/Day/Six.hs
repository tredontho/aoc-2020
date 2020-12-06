module Day.Six where

import Data.List (foldl1')
import qualified Data.Set as Set
import Paths_aoc2020 (getDataFileName)

inputFileName :: IO FilePath
inputFileName = getDataFileName "resources/six.txt"

-- Convert to list of lines
-- Break on empty lines (indicates a group)
-- For each group, create a set
-- Return length of set

input :: IO String
input = inputFileName >>= readFile

solveInput = solve <$> input

solve :: String -> Int
solve = sum . map countAnyAnswers . toGroups . cleanInput

solve2 :: String -> Int
solve2 = sum . map countAllAnswers . toGroups . cleanInput

cleanInput :: String -> [String]
cleanInput = lines

toGroups :: [String] -> [[String]]
toGroups = splitOn ""

countAnyAnswers :: [String] -> Int
countAnyAnswers = Set.size . Set.fromList . concat

countAllAnswers :: [String] -> Int
countAllAnswers = Set.size . foldl1' Set.intersection . map Set.fromList

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = first : splitOn x rest
  where
    (first, rest) = case break (== x) xs of
      (f, _ : more) -> (f, more)
      x -> x

test =
  unlines
    [ "abc",
      "",
      "a",
      "b",
      "c",
      "",
      "ab",
      "ac",
      "",
      "a",
      "a",
      "a",
      "a",
      "",
      "b"
    ]
