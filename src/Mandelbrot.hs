module Mandelbrot where

import Data.Complex
import Data.Maybe
import Data.Word
import qualified Data.Array.Repa as R
import Data.Array.Repa hiding (map,zip,foldr)

mandelbrot :: Complex Double -> Complex Double -> Complex Double
mandelbrot c z = z*z + c

isMandelbrot :: Int -> Complex Double -> Maybe Int
isMandelbrot n c = case (n' < n) of
                     True -> Just n'
                     False -> Nothing
  where
    m = iterate (mandelbrot c) 0
    n' = length . take n . takeWhile (\z -> magnitude z < 2) $ m

isMandelbrot2 :: Int -> Complex Double -> Int
isMandelbrot2 n c = case (n' < n) of
                      True ->   n'
                      False -> (-1)
  where
    m = iterate (mandelbrot c) 0
    n' = length. take n . takeWhile (\z -> magnitude z <2) $ m

array :: Double -> Complex Double -> Int -> Array D DIM2 (Complex Double)
array size centre n = fromFunction (Z:.n:.n) posToVal
  where
    dx = size / (fromIntegral n)
    posToVal (Z:.i:.j) = (((fromIntegral i)*dx - size/2) :+ ((fromIntegral j)*dx - size/2)) + centre  

genMandelbrot :: Int -> Int -> Double -> Complex Double -> Array D DIM2 (Word8,Word8,Word8)
genMandelbrot res n size centre = cs
  where
    zs = array size centre res
    cs = R.map (toColor2 . isMandelbrot2 n) zs
    --cs = R.map toColor ms

toColor2 :: Int -> (Word8,Word8,Word8)
toColor2 n = case (n==(-1)) of
               True  -> (0,0,0)
               False -> (255 - (fromIntegral n),fromIntegral n,50)


toColor :: Maybe Int -> (Word8,Word8,Word8)
toColor (Just n) = (255-(fromIntegral n),(fromIntegral n),(fromIntegral n))
toColor Nothing  = (0,0,0)

toBw2 :: Int -> (Word8, Word8, Word8)
toBw2 n = case (n==(-1)) of
            True  -> (0,0,0)
            False ->(255-n', 255-n', 255-n')
  where n' = fromIntegral n

toBw :: Maybe Int -> (Word8,Word8,Word8)
toBw (Just n') = let n = fromIntegral n' in (255-n,255-n,255-n)
toBw Nothing = (0,0,0)

