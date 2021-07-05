import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable

solve :: [String] -> Point
solve xs = findStart grid
  where w = length $ head xs
        h = length xs
        v = runSTUArray $ newListArray (0, w*h - 1) $ concat xs
        grid = Grid w h v

data Grid = Grid Int Int (UArray Int Char)
          deriving (Show)

type Point = (Int, Int)

charAt :: Grid -> Point -> Char
charAt (Grid w _ v) (x, y) = v ! (y*w + x)

findStart :: Grid -> Point
findStart (Grid w _ v) = (div i w, mod i w)
  where Just (i, _) = find ((== '@') . snd) $ assocs v

main = do
  input <- getContents
  let grid = solve $ lines input
  putStrLn $ show . solve $ lines input
