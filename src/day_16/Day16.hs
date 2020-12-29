module Day16 where

partOne _ = map (take 10) $ map pattern [1, 2, 3]

basePattern = [0, 1, 0, -1]

pattern n = drop 1 $ cycle $ concatMap (replicate n) basePattern

main = interact $ show . partOne . parse

parse :: String -> [Int]
parse = map read . map (:[]) . head . lines
