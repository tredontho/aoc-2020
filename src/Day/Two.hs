{-# LANGUAGE RecordWildCards #-}
module Day.Two where

import Paths_aoc2020 (getDataFileName)

inputFileName :: IO FilePath
inputFileName = getDataFileName "resources/two.txt"

input :: IO [String]
input = do
  raw <- inputFileName >>= readFile
  return . lines $ raw

solve :: IO Int
solve = solution <$> input

solution :: [String] -> Int
solution = length . filter checkLine . map parse

data Line = Line {lower :: Int, upper :: Int, letter :: Char, password :: String}
  deriving Show

parse :: String -> Line
parse s = mkLine $ words s
  where
    mkLine [range, c, pwd] = Line {lower = fst . parseRange $ range, upper = snd . parseRange $ range, letter = head c, password = pwd}

parseRange :: String -> (Int, Int)
parseRange range = (l, u)
  where
    (lStr, uStr) = break (== '-') range
    l = read lStr
    u = read . tail $ uStr

checkLine :: Line -> Bool
checkLine Line{..} = occurrences <= upper && occurrences >= lower
  where occurrences = length $ filter (== letter) password
