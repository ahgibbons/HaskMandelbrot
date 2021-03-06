import Data.Complex
import Data.Maybe
import Data.Word
import qualified Data.Array.Repa as R
import Data.Array.Repa hiding (map,zip,foldr)

f :: Complex Double -> Complex Double -> Complex Double
f c z = z*z + c

isMandelbrot :: Int -> Complex Double -> Int
isMandelbrot n c = case (n' < n) of
                      True ->   n'
                      False -> (-1)
  where
    m = iterate (f c) 0
    n' = length. take n . takeWhile (\z -> magnitude z <2) $ m


array :: Double -> Complex Double -> Int -> Array D DIM2 (Complex Double)
array size centre n = R.fromFunction (Z:.n:.n) posToVal
  where
    dx = size / (fromIntegral n)
    posToVal (Z:.i:.j) = (((fromIntegral i)*dx - size/2) :+ ((fromIntegral j)*dx - size/2)) + centre  

genMandelbrot :: Int -> Int -> Double -> Complex Double -> Array D DIM2 (Word8,Word8,Word8)
genMandelbrot res n size centre = cs
  where
    zs = array size centre res
    cs = R.map (toColor . isMandelbrot n) zs

toColor :: Int -> (Word8,Word8,Word8)
toColor n = case (n<0) of
               True = (0,0,0)
               False = (255 - (fromIntegral n),fromIntegral n,50)

main :: IO ()
main = do
  [i',j',size',res',n',filename] <- getArgs
  let res    = read res'     :: Int
      n      = read n'       :: Int
      size   = read size'    :: Double
      i      = read i'       :: Double
      j      = read j'       :: Double
      centre = i :+ j
      cs' = genMandelbrot res n size centre
  cs <- computeP cs' :: IO (Array U DIM2 (Word8,Word8,Word8))
  writeImageToBMP filename cs
