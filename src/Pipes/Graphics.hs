{-# LANGUAGE RankNTypes #-}

module Pipes.Graphics where

import Codec.FFmpeg
import Codec.Picture
import Codec.Picture.Png
import Control.Monad.Identity
import Control.Monad.Trans

import Data.Array.Repa hiding ((++))
import Data.Vector (convert)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Linear

import Pipes
import Pipes.Safe

import Text.Printf

produceTestImage :: Int -> Int -> Word8 -> Image PixelRGB8
produceTestImage ydim xdim w =
  let
    f (Z :. y :. x) = V3 0 0 w
  in repaToImage $ runIdentity $ computeUnboxedP $ fromFunction (Z :. ydim :. xdim) f

repaToImage :: Array U DIM2 (V3 Word8) -> Image PixelRGB8
repaToImage arr =
  let
    (Z :. ydim :. xdim) = extent arr
    v = convert . toUnboxed $ arr :: VS.Vector (V3 Word8)
  in
    Image ydim xdim . VS.unsafeCast $ v

imageProducer
  :: Monad m
  => Int
  -> Int
  -> Producer' (Image PixelRGB8) m ()
imageProducer ydim xdim =
  do
    forM_ [0,0.1..10*pi] $
      yield . produceTestImage ydim xdim . round . (*255) . abs . cos . (*(1/10/pi*2*pi))

data FFmpegOpts =
  FFmpegOpts
  { _ffmpegWidth :: Int
  , _ffmpegHeight :: Int
  , _ffmpegFps :: Int
  , _ffmpegFilePath :: FilePath
  } deriving (Show, Read)

pngWriter :: MonadIO m => Int -> FilePath -> String -> Consumer' (Image PixelRGB8) m ()
pngWriter numZeros fp prefix =
  forM_ [(0::Int)..]
  (\i ->
      do
        let fileName = printf (fp ++ "/" ++ prefix ++ "%0" ++ show numZeros ++ "d.png") i
        image <- await
        liftIO $ writePng fileName image
  )

ffmpegWriter
  :: (MonadSafe m, MonadIO m)
  => FFmpegOpts
  -> Consumer' (Image PixelRGB8) m ()
ffmpegWriter (FFmpegOpts w' h' fps fp) =
  do
    let w = fromIntegral w'
        h = fromIntegral h'
    liftIO $ initFFmpeg
    let params =
          (defaultParams w h)
          {epFps = fps}
    w <- liftIO $ imageWriter params fp
    (forever $ do
        i <- await
        liftIO $ w . Just $ i
      ) `finally` (liftIO $ w Nothing)
