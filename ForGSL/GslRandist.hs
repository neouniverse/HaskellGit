{-******************************************
  *     File Name: GslRandist.hs
  *        Author: neouniverse
  * Last Modified: 2014/06/20 20:06:11
  *******************************************-}

module HaskellGit.ForGSL.GslRandist (
   gslRanGaussian
  ,gslRanRayleigh
  ,gslRanFlat
  ,gslRanTdist
) where

import Control.Monad as CM
import System.Cmd as SC
import System.Process as SP

{--  実行速度  --}
--   100,000点
--     1.391u 0.018s 0:01.37 102.1%    0+0k 0+1808io 0pf+0w  
-- 1,000,000点
--    13.856u 0.168s 0:13.60 103.0%   0+0k 0+18048io 0pf+0w


---- Gauss乱数
--   num: データ点数
-- sigma: 標準偏差
gslRanGaussian :: Int -> Double -> IO [Double]
gslRanGaussian num sigma = do
  seed <- SP.readProcess "date" ["+%s"] []
  let cmd_string = [seed, (show num), "gaussian", (show sigma)]
  CM.liftM ((map read).lines) $ SP.readProcess "gsl-randist" cmd_string []

---- Rayleigh乱数
--   num: データ点数
-- sigma: 最頻値
gslRanRayleigh :: Int -> Double -> IO [Double]
gslRanRayleigh num sigma = do
  seed <- SP.readProcess "date" ["+%s"] []
  let cmd_string = [seed, (show num), "rayleigh", (show sigma)]
  CM.liftM ((map read).lines) $ SP.readProcess "gsl-randist" cmd_string []
  
---- 一様乱数
-- num: データ点数
-- min: 最小値
-- max: 最大値
gslRanFlat :: Int -> Double -> Double -> IO [Double]
gslRanFlat num min max = do
  seed <- SP.readProcess "date" ["+%s"] []
  let cmd_string = [seed, (show num), "flat", (show min), (show max)]
  CM.liftM ((map read).lines) $ SP.readProcess "gsl-randist" cmd_string []

---- student-t乱数
-- num: データ点数
--  nu: 自由度
gslRanTdist :: Int -> Double -> IO [Double]
gslRanTdist num nu = do
  seed <- SP.readProcess "date" ["+%s"] []
  let cmd_string = [seed, (show num), "tdist", (show nu)]
  CM.liftM ((map read).lines) $ SP.readProcess "gsl-randist" cmd_string []

{--  Test code  --}
-- main :: IO ()
-- main = do
--   print =<< gslRanGaussian 10 1.0
--   sleepCmd 1
--   print =<< gslRanGaussian 10 1.0
--   print =<<  gslRanRayleigh 10 1.0
--   print =<<  gslRanFlat 10 (-1.0) 1.0
--   print =<<  gslRanTdist 10 10.0

-- sleepCmd :: Int -> IO ()
-- sleepCmd sec = do
--   SC.system $ "sleep " ++ (show sec)
--   return ()
