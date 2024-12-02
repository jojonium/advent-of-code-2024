module Day02 (main) where

main :: IO ()
main = do
  reports <- map (map read . words) . lines <$> getContents
  putStrLn $ "Part 1: " ++ show (length (filter isSafe reports))
  putStrLn $ "Part 2: " ++ show (length (filter (any isSafe . variations) reports))


isSafe :: [Int] -> Bool
isSafe levels = (increasing || decreasing) && gradual
  where steps      = zipWith subtract levels (tail levels)
        decreasing = all (< 0) steps
        increasing = all (> 0) steps
        gradual    = all (\x -> abs x <= 3) steps


-- The original list, plus each variation of removing one element
variations :: [a] -> [[a]]
variations xs = xs : [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

