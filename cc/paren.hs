{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.ByteString (ByteString)
-- import Data.ByteString qualified as BS
-- import Data.ByteString.Builder qualified as B
-- import Data.ByteString.Builder (Builder)
-- import Data.ByteString.Lazy qualified as L

-- findIndices :: Word8 -> L.ByteString -> ([Int], [Int])
-- findIndices (w, w') bs = go 0 bs
--   where
--     go :: Int -> ByteString -> ([Int], [Int])
--     go ix bs = case L.uncons bs of
--         Nothing -> ([], [])
--         Just (c, cs) ->
--           let (is, is') = go (ix + 1) cs
--            in if c == w
--                 then (ix : is, is')
--                 else if c == w'
--                   then (is, ix : is')
--                   else (is, is')

-- scanBalance :: L.ByteString -> [Int]
-- scanBalance = (go 0 0) <$> L.length <*> findIndices (40, 41)
--     where
--         -- ix, balance, length
--         go :: Int -> Int -> Int -> ([Int], [Int]) -> [Int]
--         go i bal n = \case
--             ([], []) -> replicate (n - i) bal
--             (xs, ys) ->
--                 if not (null xs) && (head xs == i)
--                     then (bal + 1) : go (i + 1) (bal + 1) n (tail xs, ys)
--                     else if not (null ys) && (head ys == i)
--                         then (bal - 1) : go (i + 1) (bal - 1) n (xs, tail ys)
--                         else bal : go (i + 1) bal n (xs, ys)

-- isBalanced :: L.ByteString -> Bool
-- isBalanced bs = all (>= 0) (scanBalance bs) && (last (scanBalance bs) == 0)

-- -- | Returns all maximal-length strings obtained by selectively deleting open and close
-- -- parentheses from the input string until all groups are balanced
-- balance :: ByteString -> [Builder]
-- balance bs = go 0 bs
--     where
--         go :: Int -> ByteString -> [Builder]
--         go i bs =
--             let (g, rest) = span (>= 0) $ scanBalance bs
--              in if null rest
--                     then [B.byteString $ BS.take (i + length g) bs]
--                     else let (open, close) = findIndices (40, 41) $ L.fromStrict $ BS.drop (i + length g) bs
--                                 in if null open && null close
--                                     then [B.byteString $ BS.take (i + length g) bs]
--                                         else let open' = if null open then [] else tail open
--                                                 close' = if null close then [] else tail close
--                                                 bs' = BS.drop (i + length g) bs
--                                             in (B.byteString $ BS.take (head open) bs') : go (i + head open + 1) (BS.drop (head open + 1) bs')

main :: IO ()
main = do
  --   line <- BS.getLine
  --   let bs = BS.filter (`elem` "()") line
  --   if isBalanced bs
  --     then B.hPutBuilder stdout $ B.byteString bs
  --     else putStrLn "unbalanced"
  return ()