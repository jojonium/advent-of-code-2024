module Day06 (main) where

import Control.Monad.State (State, get, put, evalState, execState)
import qualified Data.Map as M

type Coord   = (Int, Int)
data Dir     = North | East | South | West deriving (Eq, Show)
type Chart   = M.Map Coord Char
type Visited = M.Map Coord [Dir]


main :: IO ()
main = do
  (chart, guardPos) <- parse . lines <$> getContents
  let (_, _, v) = execState (move chart) (guardPos, North, M.empty)
  putStrLn $ "Part 1: " ++ show (M.size v)
  putStrLn $ "Part 2: " ++ show (part2 chart guardPos (M.keys v))


-- parses input into map of (x, y) coord to char, and also returns the starting
-- position of the guard
parse :: [String] -> (Chart, Coord)
parse ls = foldr outer (M.empty, (0, 0)) (zip ls [0..])
  where outer (l, j) (m, g) = foldr (inner j) (m, g) (zip l [0..])
        inner j (c, i) (m, g) = (M.insert (i, j) c m, if c == '^' then (i, j) else g)


-- Returns True if the guard exited the map, False if it got stuck in a loop.
-- The final state contains the Map of visited coords.
move :: Chart -> State (Coord, Dir, Visited) Bool
move chart = do
  (g, d, v) <- get
  let v' = M.insertWith (++) g [d] v
      g' = moveForward d g
      (g'', d') = if chart M.! g' == '#' then (g, turnRight d) else (g', d)
  put (g'', d', v')
  if g' `M.notMember` chart then do return True  -- off the edge
  else if g'' `M.member` v && d' `elem` v M.! g'' then do return False  -- loop
  else do move chart


-- options are all the coords visited in the part 1
-- brute force ftw
part2 :: Chart -> Coord -> [Coord] -> Int
part2 chart guardPos options = length $ filter try options
  where try g = not $ evalState (move (M.insert g '#' chart)) (guardPos, North, M.empty)


turnRight :: Dir -> Dir
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North


moveForward :: Dir -> Coord -> Coord
moveForward North (x, y) = (x, y - 1)
moveForward East  (x, y) = (x + 1, y)
moveForward South (x, y) = (x, y + 1)
moveForward West  (x, y) = (x - 1, y)
