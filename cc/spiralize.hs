{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, spiralize) where

import Data.Vector (Vector, fromList, length, slice, toList, (!))
import Data.Vector qualified as V
import Prelude hiding (length)

rev = reverse

data MatrixView a = MatrixView
  { matrix :: Vector (Vector a),
    hrOffset :: Int,
    trOffset :: Int,
    hcOffset :: Int,
    tcOffset :: Int
  }

empty :: MatrixView a -> Bool
empty (MatrixView {..}) = hrOffset + trOffset >= length (matrix ! 0) || hcOffset + tcOffset >= length matrix

headRow :: MatrixView a -> Vector a
headRow (MatrixView {..}) =
  let fullRow = (matrix ! hrOffset)
   in slice hcOffset (length fullRow - (hcOffset + tcOffset)) fullRow

headCol :: MatrixView a -> Vector a
headCol (MatrixView {..}) =
  let fullCol = V.map (! hcOffset) matrix
   in slice hrOffset (length fullCol - (hrOffset + trOffset)) fullCol

lastRow :: MatrixView a -> Vector a
lastRow (MatrixView {..}) =
  let fullRow = matrix ! (length matrix - 1 - trOffset)
   in slice hcOffset (length fullRow - (hcOffset + tcOffset)) fullRow

lastCol :: MatrixView a -> Vector a
lastCol (MatrixView {..}) =
  let fullCol = V.map (\row -> row ! (length row - 1 - tcOffset)) matrix
   in slice hrOffset (length matrix - (hrOffset + trOffset)) fullCol

tighten :: MatrixView a -> MatrixView a
tighten m =
  m
    { hrOffset = hrOffset m + 1,
      hcOffset = hcOffset m + 1,
      trOffset = trOffset m + 1,
      tcOffset = tcOffset m + 1
    }

spiralize :: [[Int]] -> [Int]
spiralize xss = go mat
  where
    mat = MatrixView {matrix = fromList $ map fromList xss, hcOffset = 0, hrOffset = 0, tcOffset = 0, trOffset = 0}
    go :: MatrixView Int -> [Int]
    go m =
      if empty m
        then []
        else
          (toList . headRow $ m)
            ++ if empty m'
              then []
              else
                (drop 1 . toList . lastCol $ m')
                  ++ if empty m''
                    then []
                    else
                      (rev . drop 1 . toList . lastRow $ m'')
                        ++ if empty m'''
                          then []
                          else
                            (drop 1 . rev . drop 1 . toList . headCol $ m''')
                              ++ go (tighten m)
      where
        m' = m {hrOffset = hrOffset m + 1}
        m'' = m' {tcOffset = tcOffset m' + 1}
        m''' = m'' {trOffset = trOffset m'' + 1}

main :: IO ()
main = do
  raw <- getContents
  let xss = read raw :: [[Int]]
  print $ spiralize xss
