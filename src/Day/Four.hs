module Day.Four where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Paths_aoc2020

requiredFields :: Set String
requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

type Passport = Map String String

inputFileName :: IO FilePath
inputFileName = getDataFileName "resources/four.txt"

input :: IO [Passport]
input = do
  raw <- inputFileName >>= readFile
  let passports = makePassports . lines $ raw
  return passports

solve :: IO Int
solve = solution <$> input

solution :: [Passport] -> Int
solution passports = length $ filter validPassport passports

solve2 :: IO Int
solve2 = solution2 <$> input

solution2 :: [Passport] -> Int
solution2 passports = length $ filter strictValidation passports

makePassports :: [String] -> [Passport]
makePassports [] = []
makePassports xs = makePassport raw : makePassports rest
  where
    (current, rest) = case break null xs of
      (y, _ : ys) -> (y, ys)
      (y, []) -> (y, [])
    raw = words . unwords $ current

-- Takes inputs in key:value
makePassport :: [String] -> Passport
makePassport = Map.fromList . map toTuple
  where
    toTuple = (\(x, _ : y) -> (x, y)) . break (== ':')

missingFields :: Passport -> Set String
missingFields passport = requiredFields Set.\\ Map.keysSet passport

validPassport :: Passport -> Bool
validPassport passport = null $ Set.delete "cid" missing
  where
    missing = missingFields passport

strictValidation :: Passport -> Bool
strictValidation passport = validPassport passport && (all checkField . Map.assocs $ passport)

checkField :: (String, String) -> Bool
checkField ("byr", byr) = length byr == 4 && between (1920, 2002) year
  where
    year = read byr
checkField ("iyr", iyr) = length iyr == 4 && between (2010, 2020) year
  where
    year = read iyr
checkField ("eyr", eyr) = length eyr == 4 && between (2020, 2030) year
  where
    year = read eyr
checkField ("hgt", hgt) = validHeight (read quantity) unit
  where
    (quantity, unit) = span isDigit hgt
checkField ("hcl", hcl) = case hcl of
  '#' : xs -> validHairColor xs
  _ -> False
checkField ("ecl", ecl) = validEyeColor ecl
checkField ("pid", pid) = length pid == 9 && all isDigit pid
checkField _ = True

validHeight :: Int -> String -> Bool
validHeight q "cm" = between (150, 193) q
validHeight q "in" = between (59, 76) q
validHeight _ _ = False

validHairColor :: String -> Bool
validHairColor colo = length colo == 6 && all (Set.member `flip` validChars) colo
  where
    validChars = Set.fromList $ ['0' .. '9'] ++ ['a' .. 'f']

validEyeColor :: String -> Bool
validEyeColor = Set.member `flip` Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

between :: (Int, Int) -> Int -> Bool
between (low, high) val = val >= low && val <= high
