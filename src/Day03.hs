module Day03 (main) where

import Text.Regex.TDFA
import Data.List (isPrefixOf)

main :: IO ()
main = do
  input <- getContents
  let regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
  putStrLn $ "Part 1: " ++ show (part1 input regex)
  putStrLn $ "Part 2: " ++ show (part2 input ("\\`" ++ regex) True)


part1 :: String -> String -> Int
part1 s regex = case s =~ regex :: (String, String, String, [String]) of
  (_, _, next, [a, b]) -> read a * read b + part1 next regex
  _                    -> 0


part2 :: String -> String -> Bool -> Int
part2 "" _ _ = 0
part2 s regex enabled
  | "do()"    `isPrefixOf` s = part2 (drop 4 s) regex True
  | "don't()" `isPrefixOf` s = part2 (drop 7 s) regex False
  | s =~ regex = let (x, s') = case (enabled, s =~ regex :: (String, String, String, [String])) of
                                 (True, (_, _, next, [a, b])) -> (read a * read b, next) 
                                 (_, (_, _, next, _))         -> (0, next)
                 in x + part2 s' regex enabled
  | otherwise = part2 (drop 1 s) regex enabled

