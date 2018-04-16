{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.LLVM.PTX
import Data.Function (on)

import Data.Word (Word8)

import Linear

import Pipes
import qualified Pipes.Prelude as Pipes (take)
import Pipes.Graphics
import Pipes.Graphics.Accelerate
import Pipes.Safe

import Prelude as P

main :: IO ()
main =
  do
    let
      ydim = 100
      xdim = 100
      dim = Z :. ydim :. xdim
      !image = testAccelerateImage dim 0
      ffmpegOpts = FFmpegOpts ydim xdim 60 "test.mp4"
    putStrLn $ "Writing one test png image"
    runEffect $
      yield image >-> (await >>= yield . flatToImage dim) >-> pngWriter 3 "test"

    putStrLn $ "Writing 480 frame video (4s)"
    runSafeT $ runEffect $
      testAccelerateImageProducer dim >->
      Pipes.take 480 >->
      forever (await >>= yield . flatToImage dim) >->
      ffmpegWriter ffmpegOpts

    putStrLn $ "Showing 480 frames to OpenGL frame (4s, upper left corner)"
    runSafeT $ runEffect $
      testAccelerateImageProducer dim >->
      Pipes.take 480 >->
      forever (await >>= yield >> liftIO (threadDelay 1666)) >->
      openGLConsumerFlat dim

testAccelerateImageProducer
  :: Monad m
  => DIM2
  -> Producer' (Array DIM1 Word8) m ()
testAccelerateImageProducer dim =
  do
    forM_ ([0,0.01..] :: [Double]) $
      (\d ->
         let
           b = P.truncate . (*255) . abs $ cos d
         in
           yield $ testAccelerateImage dim b
      )

testAccelerateImage :: DIM2 -> Word8 -> Array DIM1 Word8
testAccelerateImage dim@(Z :. ydim :. xdim) b =
  run1 arrayToFlat $ fromFunction dim
  (\(Z :. y :. x) ->
      let
        y' = P.truncate . (*255) $ (((/) `on` P.fromIntegral) y ydim :: Float)
        x' = P.truncate . (*255) $ (((/) `on` P.fromIntegral) x xdim :: Float)
      in V3 y' x' b
  )
