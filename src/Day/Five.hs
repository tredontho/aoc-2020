module Day.Five where

import Data.Function ((&))
import Data.List (sort)
import Paths_aoc2020 (getDataFileName)

inputFileName :: IO FilePath
inputFileName = getDataFileName "resources/five.txt"

input :: IO [String]
input = do
  raw <- inputFileName >>= readFile
  return . lines $ raw

solve :: [String] -> Int
solve = maximum . map findSeat

solveInput :: IO Int
solveInput = solve <$> input

solve2 :: [String] -> Int
solve2 passes = (\(x, _) -> x + 1) . head . filter (\(_, diff) -> diff == 2) $ diffs
  where
    allSeats = sort . map findSeat $ passes
    diffs = zipWith (\x y -> (x, y - x)) allSeats (tail allSeats)

data Bisection = First | Second
  deriving (Show, Eq)

bisect :: Bisection -> [a] -> [a]
bisect First xs = take (half . length $ xs) xs
bisect Second xs = drop (half . length $ xs) xs

half :: Int -> Int
half = (`div` 2)

toBisection :: Char -> Bisection
toBisection 'F' = First
toBisection 'B' = Second
toBisection 'L' = First
toBisection 'R' = Second

findSeat pass = row * 8 + column
  where
    row = findRow $ take 7 pass
    column = findColumn $ drop 7 pass

findRow :: String -> Int
findRow pass = head $ foldl (&) [0 .. 127] bisections
  where
    bisections = map (bisect . toBisection) pass

findColumn :: String -> Int
findColumn pass = head $ foldl (&) [0 .. 7] bisections
  where
    bisections = map (bisect . toBisection) pass

testOne = "BFFFBBFRRR"

testTwo = "FFFBBBFRRR"

testThree = "BBFFBBFRLL"
