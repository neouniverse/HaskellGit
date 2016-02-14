
module WrapHasKAL.ForSignalProcessingUtils (
   lpfIIR
  ,hpfIIR
) where

import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.WaveUtils.Data (WaveData(..), mkWaveData)


lpfIIR :: Int -> Double -> WaveData -> WaveData
lpfIIR n fc w = iirWaveData x w
  where x = butter n (samplingFrequency w) fc Low

hpfIIR :: Int -> Double -> WaveData -> WaveData
hpfIIR n fc w = iirWaveData x w
  where x = butter n (samplingFrequency w) fc High

iirWaveData :: ([Double], [Double]) -> WaveData -> WaveData
iirWaveData x w = mkWaveData (detector w) (dataType w) (samplingFrequency w) (startGPSTime w) (stopGPSTime w) v
  where v = iir x $ gwdata w

