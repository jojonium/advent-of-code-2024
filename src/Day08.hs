module Day08 (main) where

import qualified Data.Map as M
import qualified Data.Set as S


type Coord = (Int, Int)
type Chart = M.Map Char [Coord]


main :: IO ()
main = do
  (w, h, chart) <- parse . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (solve True  w h chart)
  putStrLn $ "Part 2: " ++ show (solve False w h chart)


-- create a map of each frequency to the coordinates of all antennas of that frequency
parse :: [String] -> (Int, Int, Chart)
parse ls = (length ls, length (head ls), foldr outer M.empty (zip ls [0..]))
  where outer (l, j) m = foldr (inner j) m (zip l [0..])
        inner j (c, i) m = if c /= '.' then M.insertWith (++) c [(i, j)] m else m


antinodes :: Bool -> Int -> Int -> Char -> Coord -> [Coord] -> [Coord]
antinodes _ _ _ _ _ [] = []
antinodes p1 w h c (x, y) ((i, j):as)
  | x == i && y == j = antinodes p1 w h c (x, y) as  -- can't make an antinode with yourself
  | otherwise = myANs ++ antinodes p1 w h c (x, y) as
    where -- all points on the line intersecting (x, y) and (i, j)
          onLine = zip (if i < x then [x, x + x - i .. w - 1] else [x, x + x - i .. 0])
                       (if j < y then [y, y + y - j .. h - 1] else [y, y + y - j .. 0])
          -- in Part 1 we only want the first antinode beyond this antenna
          myANs  = if p1 then [onLine !! 1 | length onLine >= 2] else onLine


solve :: Bool -> Int -> Int -> Chart -> Int
solve p1 w h = S.size . M.foldrWithKey f S.empty 
  where f c as s = foldr (\a s' -> foldr S.insert s' (antinodes p1 w h c a as)) s as

