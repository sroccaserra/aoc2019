module Day16 where

import Data.Array.IArray

---
-- Part one

partOne = toNum . take 8 . last . take 101 . iterate applyPhase

applyPhase xs = zipWith ($) transforms (repeat xs)
  where transforms = map transform [1..length xs]

basePattern = [0, 1, 0, -1]

pattern = drop 1 . cycle . (`concatMap` basePattern) . replicate

transform n = (`mod` 10) . abs . sum . zipWith (*) (pattern n)

---
-- Part two

partTwo xs = map dec [0..7]
  where signal = concat $ replicate 10000 xs
        toDrop = toNum $ take 7 xs
        subSignal = drop toDrop signal
        n = length subSignal
        coeffs = elems (buildCoeffs n)
        dec n = (`mod` 10) $ sum (zipWith (\x y -> (`mod` 10) $ x*(fromIntegral y)) ((take n (repeat 0)) ++ coeffs) subSignal)

toNum = foldl1 ((+) . (*10))

-- Diagonal 99 of Pascal's Triangle, x_k = x_k-1*(99+k)/k
buildCoeffs :: Int -> Array Int Integer
buildCoeffs n = coeffs
  where coeffs = listArray (0, n-1) $ 1:[div ((coeffs!(i-1))*(99+(fromIntegral i))) (fromIntegral i) | i <- [1..n-1]]

---
-- Main

main = interact $ show . partTwo . parse

parse :: String -> [Int]
parse = map read . map (:[]) . head . lines
