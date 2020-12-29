module Day16 where

partOne = take 8 . last . take 101 . iterate applyPhase

applyPhase xs = zipWith ($) transforms (repeat xs)
  where transforms = map transform [1..length xs]

basePattern = [0, 1, 0, -1]

pattern n = drop 1 $ cycle $ concatMap (replicate n) basePattern

transform n = (`mod` 10) . abs . sum . zipWith (*) (pattern n)

main = interact $ show . partOne . parse

parse :: String -> [Int]
parse = map read . map (:[]) . head . lines
