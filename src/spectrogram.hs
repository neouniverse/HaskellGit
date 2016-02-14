
import Data.List (delete, elemIndex, intersect)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.Misc.ConfFile (readFileList, readConfFile)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.Function (mapSpectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramWaveData)
import HasKAL.WaveUtils.Function (catWaveData)

import WrapHasKAL.ForSignalProcessingUtils (lpfIIR, hpfIIR)
import WrapHasKAL.ForWaveUtils (WaveData(..), dropBothSideWaveData)

main = do
  {-- parameters --}
  args <- getArgs
  (conf, lst, oFile) <- case (length args, elemIndex "-o" args) of
                         (4, Just n) -> do
                           let arg' = delete "-o" $ delete (args!!(n+1)) args
                           return (arg'!!0, arg'!!1, args!!(n+1))
                         (2, Nothing) -> return (args!!0, args!!1, "X11")
                         (_, _) -> error "Usage: plottimeseries [-o output] conffile filelist"

  {-- read param --}
  flist <- readFileList lst
  ([ch, dtfft, lap, lowf, highf, ndim],_) <- readConfFile conf ["channel", "dtfft", "lapfft", "lowf", "highf", "ndim"] []

  {-- read data --}
  mbWd <- mapM (readFrameWaveData' KAGRA ch) flist
  let wd = case catMaybes mbWd of
            [] -> error "Can't find data."
            xs -> catWaveData xs

  {-- plot parameter --}
  let title = "Spectrogram: "++ch++" ("++z++")"
        where z = "dt_{FFT}="++dtfft++"s"
      xlabel = "time since GPS="++(show . fst . startGPSTime $ wd)++" [s]"

  {-- filter --}
  let fs = samplingFrequency wd
      filt = case (read lowf, read highf, read ndim) of
              (_, _, 0) -> id
              (low, 0.0, ndim) -> dropBothSideWaveData (ndim*2) . hpfIIR ndim low
              (0.0, high, ndim) -> dropBothSideWaveData (ndim*2) . lpfIIR ndim high
              (low, high, ndim) -> dropBothSideWaveData (ndim*4) . hpfIIR ndim low . lpfIIR ndim high

  {-- main --}
  let hfs = gwspectrogramWaveData (read lap) (read dtfft) $ filt wd
  histgram2dM LogYZ COLZ (xlabel, "frequency [Hz]", "[/Hz]") title oFile ((0,0),(0,0)) $ mapSpectrogram sqrt hfs
