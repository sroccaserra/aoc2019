module Day16 where

---
-- Part one

partOne = toNum . take 8 . last . take 101 . iterate applyPhase

applyPhase xs = zipWith ($) transforms (repeat xs)
  where transforms = map transform [1..length xs]

basePattern = [0, 1, 0, -1]

pattern = drop 1 . cycle . (`concatMap` basePattern) . replicate

transform n = (`mod` 10) . abs . sum . zipWith (*) (pattern n)

toNum = foldl1 ((+) . (*10))

---
-- Main

main = interact $ show . partOne . parse

parse :: String -> [Int]
parse = map read . map (:[]) . head . lines
