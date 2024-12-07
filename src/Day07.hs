module Day07 (main) where

import Data.Char (isDigit)

main :: IO ()
main = do
  input <- map parse . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (sum (map (uncurry part1) input))
  putStrLn $ "Part 2: " ++ show (sum (map (uncurry part2) input))


parse :: String -> (Int, [Int])
parse line = (read (takeWhile isDigit line), map read (tail (words line)))


part1 :: Int -> [Int] -> Int
part1 _ [] = 0
part1 target [a] = if a == target then target else 0
part1 target (a:b:ns)
  | part1 target ((a + b) : ns) > 0 = target
  | part1 target ((a * b) : ns) > 0 = target
  | otherwise = 0


part2 :: Int -> [Int] -> Int
part2 _ [] = 0
part2 target [a] = if a == target then target else 0
part2 target (a:b:ns)
  | a > target || b > target = 0
  | part2 target ((a + b) : ns) > 0 = target
  | part2 target ((a * b) : ns) > 0 = target
  | part2 target (read (show a ++ show b) : ns) > 0 = target
  | otherwise = 0
