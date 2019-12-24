{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day1
import Day2

main :: IO ()
main = do
    input <- readFile "./data/day1"

    putStrLn "part 1:"
    print $ solve1 input

    putStrLn "part 2:"
    print $ solve2 input

