{-# LANGUAGE TypeApplications #-}

module Day.One where

import Paths_aoc2020 (getDataFileName)

-- TODO: Extract out into its own common lib?
inputFileName :: IO FilePath
inputFileName = getDataFileName "resources/one.txt"

input :: IO [Int]
input = do
  raw <- inputFileName >>= readFile
  return . map (read @Int) $ lines raw

pairs :: [Int] -> [(Int, Int)]
pairs xs = [(x,y) | (x, ix) <- indexed, (y, iy) <- indexed, ix < iy]
  where
    indexed = zip xs [1..]

solution :: [Int] -> Int
solution = (uncurry (*)) . head . filter (\(x,y) -> x + y == 2020) . pairs

solve :: IO Int
solve = do
  solution <$> input

solve2 :: IO Int
solve2 = do
  solution2 <$> input

solution2 :: [Int] -> Int
solution2 = (\(x,y,z) -> x * y * z) . head . filter (\(x,y,z) -> x + y + z == 2020) . triples

triples :: [Int] -> [(Int, Int, Int)]
triples xs = [(x,y,z) | (x,ix) <- indexed, (y, iy) <- indexed, (z, iz) <- indexed, ix < iy, iy < iz]
  where
    indexed = zip xs [1..]
