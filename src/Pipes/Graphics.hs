{-# LANGUAGE RankNTypes #-}

module Pipes.Graphics where

import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Codec.Picture
import Control.Monad (forever, forM_, unless, when)
import Control.Monad.Trans (liftIO)

import Data.Array.Repa hiding ((++))
import Data.Vector (convert)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Linear

import Pipes
import Pipes.Safe

import Text.Printf

repaToImage :: Array U DIM2 (V3 Word8) -> Image PixelRGB8
repaToImage arr =
  let
    (Z :. ydim :. xdim) = extent arr
    v = convert . toUnboxed $ arr :: VS.Vector (V3 Word8)
  in
    Image ydim xdim . VS.unsafeCast $ v

data FFmpegOpts =
  FFmpegOpts
  { _ffmpegWidth :: Int
  , _ffmpegHeight :: Int
  , _ffmpegFps :: Int
  , _ffmpegFilePath :: FilePath
  } deriving (Show, Read)

polynomialDecaySampler
  :: Monad m
  => Int --desired number of frames
  -> Int --total consumed frames
  -> Pipe a a m ()
polynomialDecaySampler num' total' =
  let
      num = fromIntegral num' :: Double
      total = fromIntegral total' :: Double
      --order for which slope of f is exactly -1 at x = 0
      --and slope is never less than -1
      order = total / num :: Double
      f :: Int -> Int
      f x' = ceiling $ num/total**order*(-x + total)**order
        where x = fromIntegral x'
      go y x =
        do
          frame <- await
          let y' = f x
          when (y' < y) $ yield frame >> go (y-1) (x+1)
          unless (x > total') $ go y' (x+1)
  in
    go (num'+1) 0

pngWriter
  :: (PngSavable a, MonadIO m)
  => Int
  -> FilePath
  -> Consumer' (Image a) m ()
pngWriter numZeros fp =
  forM_ [(0::Int)..]
  (\i ->
      do
        let
          fileName =
            printf (fp ++ "-%0" ++ show numZeros ++ "d.png") i
        image <- await
        liftIO $ writePng fileName image
  )

ffmpegWriter
  :: (JuicyPixelFormat a, MonadSafe m, MonadIO m)
  => FFmpegOpts
  -> Consumer' (Image a) m ()
ffmpegWriter (FFmpegOpts w' h' fps fp) =
  do
    let w = fromIntegral w'
        h = fromIntegral h'
    liftIO $ initFFmpeg
    let params =
          (defaultParams w h)
          {epFps = fps}
    writer <- liftIO $ imageWriter params fp
    (forever $ do
        i <- await
        liftIO $ writer . Just $ i
      ) `finally` (liftIO $ writer Nothing)
