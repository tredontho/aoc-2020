module Day.Three

where

import Paths_aoc2020 (getDataFileName)

inputFileName :: IO FilePath
inputFileName = getDataFileName "resources/three.txt"

input :: IO [String]
input = do
  raw <- inputFileName >>= readFile
  return . map cycle . lines $ raw

shift :: Int -> Int -> [String] -> [String]
shift x y skiSlope = map (drop x) $ (drop y skiSlope)

solve :: IO Int
solve = solution 3 1 <$> input

-- Could add the x y to this
solution :: Int -> Int -> [String] -> Int
-- solution = undefined
solution x y rows = length . filter (== '#') . map (head . head) . takeWhile (not . null) . iterate (shift x y) $ rows

solve2 :: IO Int
solve2 = fmap product $ sequence $ map (<$> input) [solution 1 1, solution 3 1, solution 5 1, solution 7 1, solution 1 2] 
