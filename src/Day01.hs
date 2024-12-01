module Day01 (main) where

import Data.List (transpose, sort)

main :: IO ()
main = do
  [left, right] <- map sort . transpose . map (map read . words) . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (sum . map abs $ zipWith subtract left right)
  putStrLn $ "Part 2: " ++ show (sum $ map (\l -> l * (length . filter (== l)) right) left)

