{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib
import Mandelbrot

import System.IO
import Data.Complex
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.Matrix
import Data.Array.Repa.IO.BMP
import Data.Word
import System.Console.CmdArgs
import System.Environment (getArgs)

{-}
data Options = Options { centrePos :: Complex Double
                       , size_ :: Double
                       , res_ :: Int
                       , iterations :: Int
                       , zoomRepeats :: Int}
-}

data InOpts = InOpts { i_ :: Double
                     , j_ :: Double
                     , res_ :: Int
                     , width_ :: Double
                     , iters_ :: Int
                     , filepath_ :: String
                     , zooms_ :: Int}
            deriving (Show,Data,Typeable)

mandelbrotZoom :: String -> Complex Double -> Double -> Int -> Int -> Int -> Int -> IO ()
mandelbrotZoom _ _ _ _ _ 0 _ = return ()
mandelbrotZoom filebase centre size res n t maxt = do
    cs <- computeP $ genMandelbrot res n size centre
    writeImageToBMP (filebase++"_"++(show (maxt-t))++".bmp") cs
    putStrLn $ "Finished image "++(show (maxt-t))
    mandelbrotZoom filebase centre (size*0.8) res n (t-1) maxt
    

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
      filebase = filepath_ inopts
      centre = i :+ j
  mandelbrotZoom filebase centre size res n t t

data Sample = Sample {hello :: String}
            deriving (Show,Data,Typeable)



defopts = InOpts{ i_ = 0
               , j_ = 0
               , res_ = 100
               , width_ = 4
               , iters_ = 100
               , filepath_ = "mandelbrot"
               , zooms_ = 1}

sample = Sample{hello = def}

main :: IO ()
main = mainZoom =<< (cmdArgs $ defopts)
