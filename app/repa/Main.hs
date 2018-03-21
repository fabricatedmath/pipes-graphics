{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (forM_, forever)

import Data.Array.Repa
import Data.Function (on)

import Data.Word (Word8)

import Linear

import Pipes
import qualified Pipes.Prelude as Pipes (take)
import Pipes.Graphics
import Pipes.Safe

import Prelude as P

main :: IO ()
main =
  do
    let
      ydim = 100
      xdim = 100
      dim = Z :. ydim :. xdim
      !image = testRepaImage dim 0
      ffmpegOpts = FFmpegOpts ydim xdim 60 "test.mp4"
    putStrLn $ "Writing one test png image"
    runEffect $
      yield image >-> (await >>= yield . repaToImage) >-> pngWriter 3 "test"

    putStrLn $ "Writing 480 frame video (4s)"
    runSafeT $ runEffect $
      testRepaImageProducer dim >->
      Pipes.take 480 >->
      forever (await >>= yield . repaToImage) >->
      ffmpegWriter ffmpegOpts

testRepaImageProducer
  :: Monad m
  => DIM2
  -> Producer' (Array U DIM2 (V3 Word8)) m ()
testRepaImageProducer dim =
  do
    forM_ ([0,0.01..] :: [Double]) $
      (\d ->
         let
           b = P.truncate . (*255) . abs $ cos d
         in
           yield $ testRepaImage dim b
      )

testRepaImage :: DIM2 -> Word8 -> Array U DIM2 (V3 Word8)
testRepaImage dim@(Z :. ydim :. xdim) b =
  computeUnboxedS $ fromFunction dim
  (\(Z :. y :. x) ->
      let
        y' = P.truncate . (*255) $ (((/) `on` P.fromIntegral) y ydim :: Float)
        x' = P.truncate . (*255) $ (((/) `on` P.fromIntegral) x xdim :: Float)
      in V3 y' x' b
  )
