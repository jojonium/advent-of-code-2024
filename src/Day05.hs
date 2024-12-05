module Day05 (main) where

import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

type Page   = String
type Rules  = M.Map (Page, Page) Ordering
type Update = [Page]


main :: IO ()
main = do
  [r, u] <- map lines . splitOn "\n\n" <$> getContents
  let rules   = foldr addToRules M.empty r
      updates = map (splitOn ",") u
  putStrLn $ "Part 1: " ++ show ((sum . mapMaybe (solve rules True )) updates)
  putStrLn $ "Part 2: " ++ show ((sum . mapMaybe (solve rules False)) updates)


-- inserts forward and backward comparisons for a single rule like "47|53"
addToRules :: String -> Rules -> Rules
addToRules s m = M.insert (a, b) LT (M.insert (b, a) GT m)
  where a = takeWhile isDigit s
        b = drop 1 (dropWhile isDigit s)


comparingRules :: Rules -> Page -> Page -> Ordering
comparingRules rs x y = M.findWithDefault EQ (x, y) rs


-- returns middle pages for sorted updates if True, unsorted if False
solve :: Rules -> Bool -> Update -> Maybe Int
solve rs p1 ps
  | sorted `f` ps = Just $ read (sorted !! (length sorted `div` 2))
  | otherwise = Nothing
  where f = if p1 then (==) else (/=)
        sorted = sortBy (comparingRules rs) ps

