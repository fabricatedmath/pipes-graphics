{-# LANGUAGE RankNTypes #-}

module Main where

import Codec.Picture
import Control.Monad (forM_)
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa

import Data.Word (Word8)

import Linear

import Pipes
import Pipes.Graphics
import Pipes.Safe

main :: IO ()
main =
  do
    let p = testImageProducer 100 100
        ffmpegOpts = FFmpegOpts 200 200 60 "test.mp4"
        c = ffmpegWriter ffmpegOpts
    runSafeT $ runEffect $ p >-> c
    runSafeT $ runEffect $ p >-> pngWriter 3 "test"

testImageProducer
  :: Monad m
  => Int
  -> Int
  -> Producer' (Image PixelRGB8) m ()
testImageProducer ydim xdim =
  do
    forM_ ([0,0.1..10*pi] :: [Double]) $
      yield . produceTestImage ydim xdim .
      round . (*255) . abs . cos . (*(1/10/pi*2*pi))

produceTestImage :: Int -> Int -> Word8 -> Image PixelRGB8
produceTestImage ydim xdim w =
  let
    f (Z :. _y :. _x) = V3 0 0 w
  in repaToImage $ runIdentity $ computeUnboxedP $ fromFunction (Z :. ydim :. xdim) f
