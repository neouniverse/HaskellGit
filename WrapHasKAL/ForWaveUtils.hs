
module WrapHasKAL.ForWaveUtils (
  module E
, wavelenght
, dropBothSideWaveData
, takeWaveDataSecond
, dropWaveDataSecond
) where

import qualified Data.Vector.Storable as V (length)
import HasKAL.WaveUtils.Data as E

-- lengthが分からんとtakeは役に立たん
wavelenght :: WaveData -> Int
wavelenght w = V.length $ gwdata w

-- フィルタ後の処理
dropBothSideWaveData :: Int -> WaveData -> WaveData
dropBothSideWaveData n w = dropWaveData n $ takeWaveData m w
  where m = wavelenght w - n

takeWaveDataSecond :: Double -> WaveData -> WaveData
takeWaveDataSecond s w = takeWaveData n w
  where n = truncate $ s * (samplingFrequency w)

dropWaveDataSecond :: Double -> WaveData -> WaveData
dropWaveDataSecond s w = dropWaveData n w
  where n = truncate $ s * (samplingFrequency w)
