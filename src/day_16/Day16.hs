module Day16 where

import Data.List

main = interact $ show . partOne . parse

parse :: String -> [Int]
parse = map read . map (:[]) . head . lines

partOne = id
