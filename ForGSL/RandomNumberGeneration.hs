{-******************************************
  *     File Name: RandomNumberGeneration.hs
  *        Author: neouniverse
  * Last Modified: 2014/06/20 20:06:25
  *******************************************-}

-- Reference
---- [1] ``GSL Reference Manual'', Edition 1.14, Chapter 18

module HaskellGit.ForGSL.RandomNumberGeneration (
   GSLRng(..)
  ,GSLRngType
  ,newRng
  ,gslRngSet
  ,gslRngUniform
) where

import qualified Foreign as F
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified System.IO.Unsafe as SIOU

newtype GSLRng = ToRng (F.ForeignPtr ())
newtype GSLRngType = ToRngType (FP.Ptr())

{-- Test code --}
-- main = do
--   rng <- newRng
--   gslRngSet rng 100
--   print =<< gslRngUniform rng

{- p.284 18.3 -- Random number generator initialization -}
foreign import ccall "gsl_rng_alloc" gsl_rng_alloc :: GSLRngType -> IO (FP.Ptr ())
foreign import ccall "&gsl_rng_free" gsl_rng_free :: FP.FunPtr (FP.Ptr () -> IO ())
foreign import ccall "gsl_rng_set" gsl_rng_set :: FP.Ptr () -> FCT.CUInt -> IO ()

newRng :: IO GSLRng
newRng = newRng' gslRngMt19937

newRng' :: GSLRngType -> IO GSLRng
newRng' rType = do
  ptr <- gsl_rng_alloc rType
  cPtr <- F.newForeignPtr gsl_rng_free ptr
  return $ ToRng cPtr

gslRngSet :: GSLRng -> Integer -> IO ()
gslRngSet (ToRng cPtr) rSeed = F.withForeignPtr cPtr $ flip gsl_rng_set $ fromInteger rSeed

{- p.285 18.4 -- Sampling from a random number generator -}
foreign import ccall "gsl_rng_uniform" gsl_rng_uniform :: FP.Ptr () -> IO FCT.CDouble
gslRngUniform :: GSLRng -> IO Double
gslRngUniform (ToRng cPtr) = return.realToFrac =<< F.withForeignPtr cPtr gsl_rng_uniform

{- p.285 18.6 -- Auxiliary random number generator functions -}
-- foreign import ccall "gsl_rng_types_setup" gsl_rng_types_setup :: FP.Ptr (FP.Ptr ())
-- gslRngTypesSetup :: GSLRngType
-- gslRngTypesSetup = (ToRngType $ SIOU.unsafePerformIO $ F.peek gsl_rng_types_setup)

{- p.290 18.9 -- Random number generator algorithms -}
foreign import ccall "&gsl_rng_mt19937" gsl_rng_mt19937 :: FP.Ptr (FP.Ptr ())
gslRngMt19937 :: GSLRngType
gslRngMt19937 = (ToRngType $ SIOU.unsafePerformIO $ F.peek gsl_rng_mt19937)
