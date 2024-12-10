module Day10 (main) where

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Chart = M.Map Coord Int


main :: IO ()
main = do
  (chart, zeros) <- parse . lines <$> getContents
  let trails = map (hike chart) zeros
  putStrLn $ "Part 1: " ++ show (sum $ map (length . deduplicate) trails)
  putStrLn $ "Part 1: " ++ show (sum $ map length trails)


parse :: [String] -> (Chart, [Coord])
parse ls = foldr outer (M.empty, []) (zip ls [0..])
  where outer (l, j) (m, zs) = foldr (inner j) (m, zs) (zip l [0..])
        inner j (c, i) (m, zs) = 
          (M.insert (i, j) (read [c]) m, if c == '0' then (i, j) : zs else zs)


hike :: Chart -> Coord -> [Coord]
hike chart (x, y)
  | h == 9    = [(x, y)]
  | otherwise = concatMap (hike chart) reachable
  where h = chart M.! (x, y)
        adjacent  = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        reachable = filter (\c -> M.findWithDefault (-1) c chart == h + 1) adjacent


deduplicate :: Ord a => [a] -> [a]
deduplicate = S.toList . S.fromList


