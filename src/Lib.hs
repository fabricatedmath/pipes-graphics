{-# LANGUAGE RankNTypes #-}

module Lib where

import Codec.FFmpeg
import Codec.Picture
import Control.Monad.Identity
import Control.Monad.Trans

import Data.Array.Repa
import Data.Vector (convert)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Linear

import Pipes
import Pipes.Safe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

testProducer :: MonadSafe m => Producer' String m ()
testProducer = liftIO (print "dog") >> yield "dogs" `finally` (liftIO $ print "cats")

testConsumer :: MonadSafe m => Consumer' String m ()
testConsumer = (liftIO (print "llama") >> await >>= liftIO . print) `finally` (liftIO $ print "llamam")

produceTestImage :: Int -> Int -> Word8 -> Image PixelRGB8
produceTestImage ydim xdim w =
  let
    f (Z :. y :. x) = V3 w 0 0
    v = runIdentity $ computeUnboxedP $ fromFunction (Z :. ydim :. xdim) f :: Array U DIM2 (V3 Word8)
    v' = Image ydim xdim $ VS.unsafeCast $ (convert . toUnboxed $ v :: VS.Vector (V3 Word8))
  in
    v'

imageProducer
  :: Monad m
  => Int
  -> Int
  -> Producer' (Image PixelRGB8) m ()
imageProducer ydim xdim =
  do
    forM_ [0,0.1..10*pi] $ yield . produceTestImage ydim xdim . round . (*255) . abs . cos

ffmpegConsumer
  :: (MonadSafe m, MonadIO m)
  => Int
  -> Int
  -> Consumer' (Image PixelRGB8) m ()
ffmpegConsumer ydim xdim =
  do
    liftIO $ initFFmpeg
    let params = (defaultParams (fromIntegral ydim) (fromIntegral xdim)) {epFps = 60}
    w <- liftIO $ imageWriter params "dog.mp4"
    (forever $ do
        i <- await
        liftIO $ w . Just $ i
      ) `finally` (liftIO $ w Nothing)
