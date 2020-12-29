module PartTwo where

import Data.Map (Map, (!))
import qualified Data.Map as Map

main = interact $ show . solve . lines

solve xs = until isFilled stepMaze (Map.filter (`elem` ".o") maze, 0)
  where w = length $ head xs
        h = length xs
        maze = Map.fromList [((x, y), xs !! y !! x) | x <- [0..w-1], y <- [0..h-1]]
        isFilled = Map.null . Map.filter (== '.') . fst

stepMaze (maze, n) = (maze', succ n)
  where maze' = Map.mapWithKey (stepPoint maze) maze

stepPoint :: Map (Int, Int) Char -> (Int, Int) -> Char -> Char
stepPoint _ _ v | elem v "o# " = v
stepPoint maze (x,y) v = if isNextToOxygen then 'o' else v
  where neighbors = map (\k -> Map.findWithDefault '#' k maze) [(x, succ y), (x, pred y), (succ x, y), (pred x, y)]
        isNextToOxygen = elem 'o' neighbors
