module Day4 where

import Data.List (group)

toDigits ::Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (div n 10) ++ [n `mod` 10 ]

noDecrease :: [Integer] -> Bool
noDecrease [] = True
noDecrease [_] = True
noDecrease (x:y:ys) = x <= y && noDecrease (y:ys)

isNotDecreasing = noDecrease . toDigits

isSixDigit :: Integer -> Bool
isSixDigit = (==6) . length . toDigits

inRange :: Integer -> Integer -> Integer -> Bool
inRange a b x = a <= x && x <= b

hasDoubles :: Integer -> Bool
hasDoubles x = any ((>1) . length) (group $ toDigits x)


onlyPairs :: Integer -> Bool
onlyPairs x = all (\a -> length a `mod` 2 == 0) (group $ toDigits x)


solvePart1 :: Int
solvePart1 = length  [x | x <- [a..b], isSixDigit x, hasDoubles x, isNotDecreasing x]
    where
        a = 245182
        b = 790572

solvePart2 = [x | x <- [a..b], isSixDigit x, onlyPairs x, isNotDecreasing x]
    where
        a = 245182
        b = 790572


foo :: IO ()
foo = do
  let (start, end) = (245182, 790572)
  let elems = map show [start .. end]
  print $ length $ filter (isCandidate (2 <=)) elems
  print $ length $ filter (isCandidate (2 ==)) elems

isCandidate :: (Int -> Bool) -> String -> Bool
isCandidate adjacentCond str = anyAdjacent && alwaysIncreasing
  where
    anyAdjacent = any (adjacentCond . length) (group str)
    inPairs = zip str (tail str)
    alwaysIncreasing = all (uncurry (<=)) inPairs
