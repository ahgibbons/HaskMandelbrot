

module Main where

import Lib
import Mandelbrot
import ParseInput

import System.IO
import Data.Complex
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.Matrix
import Data.Array.Repa.IO.BMP
import Data.Word
import System.Console.CmdArgs
import System.Environment (getArgs)



mandelbrotZoom :: String -> Complex Double -> Double -> Int -> Int -> Double -> Int -> Int -> IO ()
mandelbrotZoom _ _ _ _ _ _ 0 _ = return ()
mandelbrotZoom filebase centre size res n scale t maxt = do
    cs <- computeP $ genMandelbrot res n size centre
    writeImageToBMP (filebase++"_"++(show (maxt-t))++".bmp") cs
    putStrLn $ "Finished image "++(show (maxt-t))
    mandelbrotZoom filebase centre (size*scale) res n scale (t-1) maxt
    

mainSingle :: IO ()
mainSingle = do
  [i',j',size',res',n',filename] <- getArgs
  let res = read res'      :: Int
      n = read n'          :: Int
      size = read size'    :: Double
      i = read i'          :: Double
      j = read j'          :: Double
      centre = i :+ j
      cs' = genMandelbrot res n size centre
  cs <- computeP cs' :: IO (Array U DIM2 (Word8,Word8,Word8))
  writeImageToBMP filename cs


mainZoom :: InOpts -> IO ()
mainZoom inopts = do
  --[i',j',size',res',n',filebase,t'] <- getArgs
  let res = res_ inopts
      n = iters_ inopts
      size = width_ inopts
      i = i_ inopts
      j = j_ inopts
      t = zooms_ inopts
      s = scale_ inopts
      filebase = filepath_ inopts
      centre = i :+ j
  mandelbrotZoom filebase centre size res n s t t


main :: IO ()
main = mainZoom =<< (cmdArgs $ defopts)
