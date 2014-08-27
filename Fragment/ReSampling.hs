{-******************************************
  *     File Name: ReSampling.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/07/09 20:13:46
  *******************************************-}

module HaskellGit.Fragment.ReSampling (
  reSampling
) where

import qualified HaskellGit.Fragment.Flip3param as HFF
import qualified HaskellGit.Fragment.DataHandle as HFD

{--  External Functions  --}
reSampling :: Double -> Double -> [Double] -> [Double]
reSampling fsOld fsNew dat
  | decimal /= 0.0 && fsOld > fsNew = thinOut intN $ lowPassFilter (fsLcm/fsOld) $ zeroInterp intO dat
  | decimal /= 0.0 && fsOld < fsNew = thinOut intN $ lowPassFilter (fsLcm/fsOld) $ zeroInterp intO dat
  | decimal == 0.0 && fsOld > fsNew = thinOut integer $ lowPassFilter (fsNew/fsOld) dat
  | decimal == 0.0 && fsOld < fsNew = lowPassFilter (fsNew/fsOld) $ zeroInterp integer dat
  | decimal == 0.0 && fsOld == fsNew = dat
  where intO = fst $ properFraction $ fsLcm / fsOld
        intN = fst $ properFraction $ fsLcm / fsNew
        fsLcm = getFsLcm fsOld fsNew
        integer = fst $ properFraction power
        decimal = snd $ properFraction power
        power = (max fsOld fsNew) / (min fsOld fsNew)

{--  Internal Functions  --}
thinOut :: Int -> [Double] -> [Double]
thinOut n xs = map head $ HFD.dataSplitV n 0 $ xs

zeroInterp :: Int -> [Double] -> [Double]
zeroInterp n xs = concat $ map (++ (replicate (n-1) 0.0)) $ HFD.dataSplitV 1 0 xs

getFsLcm :: Double -> Double -> Double
getFsLcm fsOld fsNew
  | oldDeci == 0.0 && newDeci == 0.0 = fromIntegral $ lcm (truncate fsOld) (truncate fsNew)
  | otherwise = error "Non-integral multiplication with non-integrer sampling is not implemented yet."
  where oldDeci = snd $ properFraction fsOld
        newDeci = snd $ properFraction fsNew


{--  Dummy Functions  --}
lowPassFilter :: Double -> [Double] -> [Double]
lowPassFilter gain dat = map (*g) $ map sum $ HFD.dataSplit n m dat'
  where dat' = (drop ((length dat)-m) dat) ++ dat
        m = n - 1
        n = (truncate $ max gain (1.0/gain))
        g = case n==(truncate gain) of True -> 1.0
                                       False -> gain

