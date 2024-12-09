module Day09 (main) where

import qualified Data.Map as M
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Debug.Trace (traceShow, trace)


-- map of memory index to block ID. -1 block ID represents empty
type Chart = M.Map Int Int


main :: IO ()
main = do
  input <- dropWhileEnd isSpace <$> getContents
  let (chart, i) = parse 0 0 M.empty input
      maxId      = length input `div` 2
  putStrLn $ "Part 1: " ++ show (checksum (compact 0 i chart))
  putStrLn $ "Part 2: " ++ show (checksum (part2 chart input maxId))


-- returns map of memory, index of last file block
parse :: Int -> Int -> Chart -> String -> (Chart, Int)
parse mi _ m "" = (m, prevFileBlock m (mi - 1))
parse mi fid m (s:ss) = parse mi' (fid + 1) m' ss
  where size = read [s] :: Int
        mi'  = mi + size
        fid' = if even fid then fid `div` 2 else -1
        m'   = foldr (`M.insert` fid') m [mi .. mi' - 1]


prettyPrint :: Chart -> String
prettyPrint = unwords . map ((\x -> if x == "-1" then "." else x) . show . snd) . M.toList


prevFileBlock :: Chart -> Int -> Int
prevFileBlock m x = if m M.! x == -1 then prevFileBlock m (x - 1) else x


compact :: Int -> Int -> Chart -> Chart
compact fp bp m
  | fp >= bp = M.filterWithKey (\k _ -> k <= fp) m -- forward pointer and backward pointer meet, done compacting
  | m M.! fp /= -1 = compact (fp + 1) bp m -- block already occupied, skip
  | otherwise = compact (fp + 1) bp' m'
  where bp' = prevFileBlock m (bp - 1)
        m'  = M.insert fp (m M.! bp) m


checksum :: Chart -> Int
checksum = sum . map (\(i, v) -> if v == -1 then 0 else i * v) .  M.toList


-- this is really slow but works
part2 :: Chart -> String -> Int -> Chart
part2 m _ fid | fid <= 0 = m
part2 m s fid = part2 m' s (fid - 1)
  where 
    fileSize = read [s !! (fid * 2)]
    m' = case firstFit m fileSize 0 fid of
      Nothing -> trace (show fid ++ " no fit") m
      Just i  -> 
        let minusFile = M.map (\x -> if x == fid then -1 else x) m
            newBlocks = [i .. i + fileSize - 1]
        in trace (show fid ++ " moving to " ++ show newBlocks) $ foldr (`M.insert` fid) minusFile newBlocks


firstFit :: Chart -> Int -> Int -> Int -> Maybe Int
firstFit m fileSize i fid
  | i `M.notMember` m = Nothing
  | m M.! i == fid    = Nothing
  | all (\x -> M.findWithDefault (-1) x m == -1) [i .. i + fileSize - 1] = Just i
  | otherwise = firstFit m fileSize (i + 1) fid


