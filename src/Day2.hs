{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.List (findIndex)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

inputData :: IO [Int]
inputData = fmap (read . T.unpack) . T.splitOn "," <$> TIO.readFile "./data/day2"

type Memory = [Int]

data Inst = Add Int Int Int
          | Mul Int Int Int
          | Halt
          deriving (Show, Eq)

getInst :: Int -> Memory -> Inst
getInst pos mem
  | op == 1 = Add arg1 arg2 dest
  | op == 2 = Mul arg1 arg2 dest
  | op == 99 = Halt
  | otherwise = error $ "Invalid opcode: " ++ show (mem !! pos)
    where
        op = mem !! pos
        arg1 = mem !! (mem !! (pos + 1))
        arg2 = mem !! (mem !! (pos + 2))
        dest = mem !! (pos + 3)

day2Part1 :: Memory -> Int
day2Part1 xs = head . runProgram 0 $ patched
    where
        patched = set (set xs 1 12) 2 2

day2Part2 :: Memory -> Maybe (Int, Int)
day2Part2 xs = findIndex hasMagicNum allPrograms >>= \x -> pure $ allCombinations !! x
    where
        allCombinations = [(x,y) | x <- [0..99], y <- [0..99]]
        allMems = fmap (\(a,b) -> set (set xs 1 a) 2 b) allCombinations
        allPrograms = runProgram 0 <$> allMems
        hasMagicNum xs = head xs == 19690720

set :: [a] -> Int -> a -> [a]
set xs i e = case splitAt i xs of
    (old, _:new) -> old ++ e : new
    _ -> xs

runProgram :: Int -> Memory -> Memory
runProgram pos mem = case getInst pos mem of
    (Add a b dest) -> runProgram (pos + 4) (set mem dest (a + b))
    (Mul a b dest) -> runProgram (pos + 4) (set mem dest (a * b))
    Halt           -> mem

