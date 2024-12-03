module Day03 (main) where

import Data.List (isPrefixOf)
import Data.Char (isDigit)

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "Part 1: " ++ show (solve input True True)
  putStrLn $ "Part 2: " ++ show (solve input False True)


-- input, isPart1, isEnabled
-- regex would be easier to write, but very slow
solve :: String -> Bool -> Bool -> Int
solve "" _ _ = 0
solve s p1 enabled
  | "do()" `isPrefixOf` s = solve (drop 4 s) p1 True
  | not enabled = solve (drop 1 s) p1 enabled
  | "don't()" `isPrefixOf` s && not p1 = solve (drop 7 s) p1 False
  | "mul(" `isPrefixOf` s =
    let s1 = drop 4 s
        a  = takeWhile isDigit s1
        s2 = dropWhile isDigit s1
        b  = takeWhile isDigit (drop 1 s2)
        s3 = dropWhile isDigit (drop 1 s2)
    in if not (null a) && length a < 4 && head s2 == ',' &&
          not (null b) && length b < 4 && head s3 == ')'
       then read a * read b + solve (drop 1 s3) p1 enabled
       else solve (drop 1 s) p1 enabled
  | otherwise = solve (drop 1 s) p1 enabled

