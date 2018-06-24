{-# LANGUAGE ScopedTypeVariables #-}

module FunctionalImage
    ( generateFunctionalImage
    ) where

import Codec.Picture
import Control.Monad.Random

type DPixel = (Double, Double)

pixelFunctions :: [DPixel -> DPixel]
pixelFunctions = [\(x, y) -> (y, x), \(x, y) -> (cos x, sin y)]

pixel2ChannelFunctions :: [DPixel -> Double]
pixel2ChannelFunctions = [\(x, y) -> x, \(x, y) -> y, \(x, y) -> (x + y) / 2]

channelFunctions :: [Double -> Double]
channelFunctions = [cos, sin, \x -> 1 / (2 - x)]

randomFunction :: (RandomGen g) => [i -> o] -> Rand g (i -> o)
randomFunction fs =  do
  i <- getRandomR (0, length fs - 1)
  return (fs !! i)

randomFixedChain :: (RandomGen g) => [t -> t] -> Int -> Rand g (t -> t)
randomFixedChain fs len = foldl (\accR fR ->
                                   do
                                     acc <- accR
                                     f <- fR
                                     return (acc .f)) (pure id) rfs
  where rfs = replicate len $ randomFunction fs

randomChain :: (RandomGen g) => [t -> t] -> Rand g (t -> t)
randomChain fs = do
  len <- getRandomR(1, 2)
  res <- randomFixedChain fs len
  return res

randomPixel2Channel :: (RandomGen g) => Rand g (DPixel -> Double)
randomPixel2Channel = do
  pf <- randomChain pixelFunctions
  cf <- randomChain channelFunctions
  p2cf <- randomFunction pixel2ChannelFunctions
  return (cf . p2cf . pf)


generateFunctionalImage :: String -> Int -> Int -> IO ()
generateFunctionalImage path resX resY =
   do
     rf <- evalRandIO randomPixel2Channel
     gf <- evalRandIO randomPixel2Channel
     bf <- evalRandIO randomPixel2Channel
     let toPixel :: Double -> Pixel8 = floor . (* 255)
     let pixelRenderer px py =
           PixelRGBA8 (toPixel $ rf (x, y)) (toPixel $ gf (x, y)) (toPixel $ bf (x, y)) 255
           where
             x :: Double = (fromIntegral px) / (fromIntegral resX)
             y :: Double = (fromIntegral py) / (fromIntegral resY)
     writePng path $ generateImage pixelRenderer resX resY
