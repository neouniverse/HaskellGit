{-******************************************
  *     File Name: IOTextData.hs
  *        Author: neouniverse
  * Last Modified: 2014/07/09 14:35:35
  *******************************************-}

module HaskellGit.Fragment.IOTextData (
   readOneColumn
  ,readMultiColumn
  ,readOneColumnI
  ,readMultiColumnI
  ,showOneColumn
  ,showMultiColumn
) where

import qualified Control.Monad as CM

readOneColumn :: (Fractional a, Read a) => String -> [a]
readOneColumn = map read.words

readMultiColumn :: (Fractional a, Read a) => String -> [[a]]
readMultiColumn = map readOneColumn.lines

readOneColumnI :: (Integral a, Read a) => String -> [a]
readOneColumnI = map (truncate.read).words

readMultiColumnI :: (Integral a, Read a) => String -> [[a]]
readMultiColumnI = map readOneColumnI.lines

showOneColumn :: (Num a, Show a) => [a] -> String
showOneColumn = unlines.map show

showMultiColumn :: (Num a, Show a) => [[a]] -> String
showMultiColumn = unlines.map (unwords.map show)

