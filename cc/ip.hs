module Main where

import Data.Char (isDigit)
import Data.List (intercalate, intersperse)
import Data.Maybe (isJust)

ipSeg :: String -> Bool
ipSeg s = not (null s) && length s <= 3 && (read s :: Int) <= 255 && null (takeWhile (== '0') s)

-- | Return the list or lack thereof, of all possible splits of the given numeric string into
--  valid IP-address. The first argument indicates how many segments we have already selected
--  at this point in the decision-tree, and therefore limits how many new segments the function
--  will attempt to split off, as well as the decision of whether any valid segmentations can be
--  found at all.
splitIp :: Int -> String -> Maybe [[String]]
splitIp n s
  | n > 4 = Nothing
  | n == 3 =
      if ipSeg s
        then Just [[s]]
        else Nothing
  | otherwise =
      let res = filter isJust $ map (\i -> go i s) [1 .. 3]
       in if null res
            then Nothing
            else (concat <$>) . sequence $ res
  where
    go :: Int -> String -> Maybe [[String]]
    go l s' =
      let (seg, rest) = splitAt l s'
       in if ipSeg seg
            then case splitIp (n + 1) rest of
              Nothing -> Nothing
              Just segs -> Just (map (seg :) segs)
            else Nothing

ip :: String -> [String]
ip s = case splitIp 0 s of
  Nothing -> []
  Just ips -> map (intercalate ".") ips

main :: IO ()
main = do
  line <- getLine
  let input = takeWhile isDigit line
  print $ ip input
