module Main where

import Data.Binary (Word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 as BC

rlEncode :: ByteString -> ByteString
rlEncode = toStrict . B.toLazyByteString . rlEncode'
  where
    rlEncode' b | BS.null b = mempty
    rlEncode' b =
      let (run, rest) = BS.span (== BS.head b) b
          runLen = BS.length run
       in go runLen (BS.head b) rest
    go :: Int -> Word8 -> ByteString -> B.Builder
    go 0 _ t = rlEncode' t
    go n w t
      | n <= 9 = B.int8Dec (fromIntegral n) <> B.word8 w <> rlEncode' t
      | otherwise = B.int8Dec 9 <> B.word8 w <> go (n - 9) w t

main :: IO ()
main = do
  input <- BS.getLine
  let encoded = rlEncode input
  BS.putStr encoded
  putChar '\n'
