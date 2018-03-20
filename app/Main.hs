{-# LANGUAGE RankNTypes #-}

module Main where

import Codec.Picture
import Codec.Picture.Png
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)
import Control.Monad.Identity (runIdentity)

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Data.Vector.Storable
import Data.Vector (convert)
import qualified Data.Vector.Storable as VS
--import Data.Array.Repa

import Data.Word (Word8)

import Linear

import Pipes
import Pipes.Graphics
import Pipes.Graphics.Accelerate
import Pipes.Safe

import Prelude as P

main :: IO ()
main =
  do
    let
      --p = testImageProducer 100 100
        --ffmpegOpts = FFmpegOpts 200 200 60 "test.mp4"
        --c = ffmpegWriter ffmpegOpts
    --runSafeT $ runEffect $ p >-> c
    --runSafeT $ runEffect $ p >-> pngWriter 3 "test"
        image' = testAccelerateImage' 100 100
        image = testAccelerateImage 100 100
        dim = A.Z A.:. 100 A.:. 100
        consumer = openGLConsumer' dim
    --writePng "dog.png" $ arrayToImage image'
    runSafeT $ runEffect $ forever (yield image' >> liftIO (threadDelay 1000000)) Pipes.>-> consumer

{-
accelerateToImage :: Array DIM2 (V3 Word8) -> Image PixelRGB8
accelerateToImage arr =
  let
     (Z :. ydim :. xdim) = arrayShape arr
     v' = toVectors $ arr :: VS.Vector (Word8)
     v = v'
  in
    Image ydim xdim . VS.unsafeCast $ v
-}
    {-
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
-}
testAccelerateImage :: Int -> Int -> A.Array A.DIM2 (Word8,Word8,Word8)
testAccelerateImage w h = A.fromList (A.Z A.:. h A.:. w) $ [ (x,y,0) | x <- P.reverse [0..99], y <- [0..99]]

testAccelerateImage' :: Int -> Int -> A.Array A.DIM2 Word32
testAccelerateImage' w h = A.fromList (A.Z A.:. h A.:. w) $ [ pack8 x y 0 255 | x <- [0..99], y <- [0..99]]
