{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Pipes.Prelude as Pipes
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

-- | Given the number of frames and total consumed frames, returns
-- a function that computes the integral from to x**c
polynomialIntegral
  :: Int --desired number of frames
  -> Int --total consumed frames
  -> Double --power
  -> (Int -> Int -> Double) --from -> to -> density
polynomialIntegral n' total' c =
  let
    n = fromIntegral n'
    total = fromIntegral total'
    --multiplier such that integral 0 to n == total
    y = (c+1)*total/(n**(c+1)) :: Double
    integral :: Int -> Int -> Double
    integral from' to' =
      let
        from = fromIntegral from'
        to = fromIntegral to'
      in y/(c+1)*(to**(c+1) - from**(c+1))
  in integral

-- | Creates a distribution such that the integral from 0 to n of x**c == total,
-- it then samples from this distribution, dropping an amount of "frames" equal
-- to the integer parts of the accumulated total
-- the result is a decayed sampler that prefers "near" frames
-- and sparsely samples "far" frames
polynomialIntegralDecaySampler
  :: forall a m. MonadIO m
  => Int --desired number of frames
  -> Int --total consumed frames
  -> Double --power
  -> Pipe a a m ()
polynomialIntegralDecaySampler n total c =
  let
    integral = polynomialIntegral n (total-n) c
    go :: MonadIO m => Double -> Int -> Pipe a a m ()
    go accum x =
      do
        accum' <-
          do
            let accum' = accum + integral (pred x) x
            if accum' > 1 then
              do
                let (i,frac) = properFraction accum'
                forM_ ([(1::Int)..(i)]) $ const await
                pure frac
              else pure accum'
        await >>= yield
        go accum' (succ x)
  in go 0 1 >-> Pipes.take n

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
