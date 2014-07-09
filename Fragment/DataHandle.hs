{-******************************************
  *     File Name: DataHandle.hs
  *        Author: neouniverse
  * Last Modified: 2014/07/09 20:14:00
  *******************************************-}

module HaskellGit.Fragment.DataHandle (
   dataSplit
  ,dataSplitV
  ,dataSplitM
) where

import qualified Data.Packed.Vector as DPV
import qualified Data.Packed.Matrix as DPM

import qualified HaskellGit.Fragment.Flip3param as HMF

-- n: chunck size
-- m: overlap

dataSplit :: Int -> Int -> [a] -> [[a]]
dataSplit n m xs = map ((take n).(flip drop xs)) [0, (n-m)..(length xs)-n]

dataSplitV :: (DPM.Element a) => Int -> Int -> [a] -> [[a]]
dataSplitV n m xs = map DPV.toList $ map (HMF.flip231 DPV.subVector n $ DPV.fromList xs) $ [0, (n-m)..(length xs)-n]

dataSplitM :: (DPM.Element a) => Int -> Int -> [[a]] -> [[[a]]]
dataSplitM n m xss = map DPM.toLists $ map (subMatrix' xm n) $ [0, (n-m)..(DPM.rows xm)-n]
  where xm = DPM.fromLists xss
        subMatrix' mat n m = DPM.subMatrix (m, 0) (n, (DPM.cols mat)) mat

