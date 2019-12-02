{-# LANGUAGE OverloadedStrings #-}

module Main where

main :: IO ()
main = do
    input <- readFile "./input"

    putStrLn "part 1:"
    print $ solve1 input

    putStrLn "part 2:"
    print $ solve2 input

--
-- Part 1
--
calc :: Float -> Integer
calc x = (floor (x / 3)) - 2

solve1 :: String -> Integer
solve1 = sum
      . fmap (calc . read)
      . lines

--
-- Part 2
--

calcRec :: Integer -> Integer
calcRec x
    | res <= 0  = 0
    | otherwise = res + calcRec res
  where
      res = calc $ fromIntegral x

solve2 :: String -> Integer
solve2 = sum
       . fmap (calcRec . read)
       . lines


