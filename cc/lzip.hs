{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

module Main (compress, main) where

import Data.ByteString (ByteString, null)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy qualified as B
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import System.Environment
import System.IO
import Prelude hiding (null)

type Patterns = Map ByteString (Set Int)

{-
    Perform Lempel-Ziv naive compression of a ByteString (max length 9)

    Lempel-Ziv (LZ) compression is a data compression technique which encodes
    data using references to earlier parts of the data. In this variant of LZ,
    data is encoded in two types of chunk. Each chunk begins with a length L,
    encoded as a single ASCII digit from 1 to 9, followed by the chunk data,
    which is either:

    1. Exactly L characters, which are to be copied directly into the
    uncompressed data.
    2. A reference to an earlier part of the uncompressed data. To do this, the
    length is followed by a second ASCII digit X: each of the L output
    characters is a copy of the character X places before it in the uncompressed
    data.

    For both chunk types, a length of 0 instead means the chunk ends immediately,
    and the next character is the start of a new chunk. The two chunk types
    alternate, starting with type 1, and the final chunk may be of either type.

    Examples (some have other possible encodings of minimal length):
        abracadabra     ->  7abracad47
        mississippi     ->  4miss433ppi
        aAAaAAaAaAA     ->  3aAA53035
        2718281828      ->  627182844
        abcdefghijk     ->  9abcdefghi02jk
        aaaaaaaaaaaa    ->  3aaa91
        aaaaaaaaaaaaa   ->  1a91031
        aaaaaaaaaaaaaa  ->  1a91041

    The function compress will perform this compression algorithm to the shortest possible overall length.
-}

initMap :: Int -> ByteString -> Patterns
initMap = initMap' M.empty
  where
    initMap' :: Patterns -> Int -> ByteString -> Patterns
    initMap' patterns ix bs
      | null bs = patterns
      | otherwise =
          let patterns' = go ix bs patterns
           in initMap' patterns' (ix + 1) (BS.tail bs)
    go :: Int -> ByteString -> Patterns -> Patterns
    go ix bs = go' ix (BS.take 9 bs)

    go' :: Int -> ByteString -> Patterns -> Patterns
    go' ix bs patterns
      | null bs = patterns
      | otherwise =
          M.alter (\case Just ixs -> Just (S.insert ix ixs); Nothing -> Just (S.singleton ix)) bs $
            go' ix (BS.init bs) patterns

findPat :: Int -> ByteString -> Patterns -> Maybe (Int, Int)
findPat ix needle haystack =
  if BS.length needle < 2
    then Nothing
    else case M.lookup needle haystack of
      Just ixs ->
        let earlier = S.filter (< ix) ixs
         in if S.size earlier > 0
              then let i = S.findMax earlier in Just (BS.length needle, i)
              else findPat ix (BS.init needle) haystack
      Nothing -> findPat ix (BS.init needle) haystack

compressChunks :: ByteString -> ByteString -> ByteString -> Int -> Patterns -> [ByteString]
compressChunks orig slice chunk ix patterns
  | null slice = [lenPref chunk | not (null chunk)]
  | otherwise =
      case findPat ix (BS.take 9 slice) patterns of
        Just (l, i) -> (if null chunk then id else (lenPref chunk :)) $ pack (show l ++ show (ix - i)) : compressChunks orig (BS.drop l slice) BS.empty (ix + l) patterns
        _ ->
          case BS.uncons slice of
            Just (h, t) ->
              if BS.length chunk >= 9
                then (if null chunk then id else (lenPref chunk :)) $ compressChunks orig t (BS.singleton h) (ix + 1) patterns
                else compressChunks orig t (chunk <> BS.singleton h) (ix + 1) patterns
            Nothing -> ([lenPref chunk | not (BS.null chunk)])

lenPref :: ByteString -> ByteString
lenPref chunk
  | null chunk = BS.empty
  | otherwise = pack (show (BS.length chunk)) <> chunk

compress :: ByteString -> ByteString
compress bs
  | null bs = BS.empty
  | otherwise =
      let chunks = compressChunks bs bs BS.empty 0 (initMap 0 bs)
          encoded = foldl (\acc chunk -> acc <> B.byteString chunk) mempty chunks
       in B.toStrict $ B.toLazyByteString encoded

minCompress :: ByteString -> ByteString
minCompress bs
  | null bs = BS.empty
  | otherwise =
      let pats = initMap 0 bs
       in let f i =
                let chunks = lenPref (BS.take i bs) : compressChunks bs (BS.drop i bs) mempty i pats
                    encoded = foldl (\acc chunk -> acc <> B.byteString chunk) mempty chunks
                 in B.toStrict $ B.toLazyByteString encoded
           in let options = map f [1 .. 9]
               in let min = foldl (\acc x -> if BS.length x < BS.length acc then x else acc) (head options) options
                   in min

main :: IO ()
main = do
  args <- getArgs
  case args of
    rawtext : _ ->
      do
        print (length rawtext)
        let comp = minCompress (pack rawtext)
        BS.putStr comp
        putChar '\n'
        print (BS.length comp)
        hFlush stdout
    _ -> putStrLn "Usage: lzip <text>"
