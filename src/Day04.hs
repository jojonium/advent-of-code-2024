module Day04 (main) where

import qualified Data.Map as M

type Coord = (Int, Int)
type Chart = M.Map Coord Char


main :: IO ()
main = do
  input <- lines <$> getContents
  let chart  = parse input
      coords = [(x, y) | x <- [0 .. length input - 1],
                         y <- [0 .. length (head input) - 1]]
  putStrLn $ "Part 1: " ++ show (sum $ map (part1 chart) coords)
  putStrLn $ "Part 2: " ++ show (length $ filter (part2 chart) coords)


-- parses input into map of (x, y) coord to char
parse :: [String] -> Chart
parse ls = foldr outer M.empty (zip ls [0..])
  where outer (l, j) m = foldr (inner j) m (zip l [0..])
        inner j (c, i) = M.insert (i, j) c


-- number of XMAS's that start from this coord
part1 :: Chart -> Coord -> Int
part1 chart (x, y) | chart M.! (x, y) /= 'X' = 0
part1 chart (x, y) = length $ filter checkDir dirs
  where dirs = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
        checkDir (dx, dy) =
          M.findWithDefault '.' (x + dx, y + dy) chart == 'M' &&
          M.findWithDefault '.' (x + 2 * dx, y + 2 * dy) chart == 'A' &&
          M.findWithDefault '.' (x + 3 * dx, y + 3 * dy) chart == 'S'


-- whether this coord is the center of an X-MAS
part2 :: Chart -> Coord -> Bool
part2 chart (x, y) | chart M.! (x, y) /= 'A' = False
part2 chart (x, y) = all (`elem` ["MS", "SM"]) [tlbr, bltr]
  where tlbr = [M.findWithDefault '.' (x - 1, y - 1) chart,
                M.findWithDefault '.' (x + 1, y + 1) chart]
        bltr = [M.findWithDefault '.' (x - 1, y + 1) chart,
                M.findWithDefault '.' (x + 1, y - 1) chart]

