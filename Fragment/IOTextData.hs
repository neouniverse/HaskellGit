{-******************************************
  *     File Name: IOTextData.hs
  *        Author: neouniverse
  * Last Modified: 2014/06/20 20:05:25
  *******************************************-}

module HaskellGit.Fragment.IOTextData (
   readOneColumn
  ,readMultiColumn
  ,showOneColumn
  ,showMultiColumn
) where

readOneColumn :: String -> [Double]
readOneColumn = map read.words

readMultiColumn :: String -> [[Double]]
readMultiColumn = map (map read.words).lines

showOneColumn :: [Double] -> String
showOneColumn = unlines.map show

showMultiColumn :: [[Double]] -> String
showMultiColumn = unlines.map (unwords.map show)

