module Day05 (main) where

import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)

type Page   = String
type Rule   = (Page, Page)
type Update = [String]


main :: IO ()
main = do
  [r, u] <- map lines . splitOn "\n\n" <$> getContents
  let rules   = map (\s -> (takeWhile isDigit s, drop 1 (dropWhile isDigit s))) r
      updates = map (splitOn ",") u
  putStrLn $ "Part 1: " ++ show ((sum . mapMaybe (part1 rules)) updates)
  putStrLn $ "Part 1: " ++ show ((sum . mapMaybe (part2 rules)) updates)


comparingRules :: [Rule] -> Page -> Page -> Ordering
comparingRules [] _ _ = EQ  -- no rule applies to these two pages
comparingRules ((a, b):rs) x y
  | a == x && b == y = LT
  | a == y && b == x = GT
  | otherwise = comparingRules rs x y


-- returns Just middle page if this update is sorted correctly, Nothing otherwise
part1 :: [Rule] -> Update -> Maybe Int
part1 rs ps
  | sortBy (comparingRules rs) ps == ps = Just $ read (ps !! (length ps `div` 2))
  | otherwise = Nothing


-- sorts and returns Just middle page if this update is NOT sorted correctly
part2 :: [Rule] -> Update -> Maybe Int
part2 rs ps
  | sorted /= ps = Just $ read (sorted !! (length sorted `div` 2))
  | otherwise = Nothing
  where sorted = sortBy (comparingRules rs) ps
