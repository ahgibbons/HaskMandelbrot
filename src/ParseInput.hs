{-# LANGUAGE DeriveDataTypeable #-}

module ParseInput where

import System.Console.CmdArgs


data InOpts = InOpts { i_ :: Double
                     , j_ :: Double
                     , res_ :: Int
                     , width_ :: Double
                     , iters_ :: Int
                     , filepath_ :: String
                     , scale_ :: Double
                     , zooms_ :: Int}
            deriving (Show,Data,Typeable)


defopts = InOpts{ i_ = 0
               , j_ = 0
               , res_ = 100
               , width_ = 4
               , iters_ = 100
               , filepath_ = "mandelbrot"
               , scale_ = 0.8
               , zooms_ = 1}
