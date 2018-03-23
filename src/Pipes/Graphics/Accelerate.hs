{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Graphics.Accelerate
  ( imageToArray, arrayToImage
  , openGLConsumer, openGLConsumer', squaredDistanceShutoff
  , pack8, packRGBTupA, packRGBVecA)
where

import Codec.Picture
import Codec.Picture.Extra (flipVertically)
import Codec.Picture.Types as J

import Control.Monad (unless, when)

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Data.Bits as A
import Data.Array.Accelerate.IO.Data.Vector.Storable
import Data.Array.Accelerate.Linear.V3
import Data.Bits as B
import qualified Data.Vector.Storable as VS

import Foreign.Ptr

import Graphics.Rendering.OpenGL as GL hiding (pixelMap)
import qualified Graphics.UI.GLFW as G

import Linear

import Pipes
import Pipes.Safe

import Prelude as P

import System.Exit

--TODO: More robust frame buffers (get rid of drawPixels, which is deprecated)
--TODO: Make rgb8 instead of rgb8 to save image space

imageToArray :: Image PixelRGBA8 -> Array DIM2 Word32
imageToArray img =
  let
    dim = Z :. J.imageHeight img :. imageWidth img
    v = imageData $ pixelMap (\(PixelRGBA8 r g b a) -> pack8 r g b a) img
      :: VS.Vector Pixel32
  in
    fromVectors dim v

arrayToImage :: Array DIM2 Word32 -> Image PixelRGBA8
arrayToImage arr =
  let
    (Z :. ydim :. xdim) = arrayShape arr
    v = toVectors arr
  in flipVertically $ Image xdim ydim . VS.unsafeCast $ v

squaredDistance
  :: Array DIM2 Word32
  -> Array DIM2 Word32
  -> Double
squaredDistance arr1 arr2 =
  let
    v1 = toVectors arr1
    v2 = toVectors arr2
    d = VS.sum $ VS.zipWith
        (\a b ->
            let
              a' = P.fromIntegral <$> unpackRGBVec a
              b' = P.fromIntegral <$> unpackRGBVec b
            in qd a' b'
        ) v1 v2
  in
    d*d

squaredDistanceShutoff
  :: MonadIO m
  => Pipe (Array DIM2 Word32) (Array DIM2 Word32) m ()
squaredDistanceShutoff =
  do
    let
      go arr1 =
        do
          arr2 <- await
          liftIO $ print $ squaredDistance arr1 arr2
          yield arr1
          go arr2
    await >>= go

openGLConsumer
  :: (MonadSafe m, MonadIO m)
  => DIM2
  -> Consumer' (Array DIM2 Word32) m ()
openGLConsumer (Z :. height :. width) =
  do
    let
      s = Size (P.fromIntegral width) (P.fromIntegral height)
    window <- liftIO $ do
      _b <- G.init
      mw@(Just window) <-
        G.createWindow width height "something" Nothing Nothing
      G.makeContextCurrent mw
      G.setCursorInputMode window $ G.CursorInputMode'Hidden
      G.setKeyCallback window $ Just keyCallback
      GL.clearColor $= Color4 0 0 0 0
      GL.shadeModel $= Flat
      GL.rowAlignment Unpack $= 1
      return window
    let
      go =
        do
          arr <- await
          close <- liftIO $
            do
              clear [ColorBuffer]
              process s arr
              GL.flush
              GL.finish
              G.swapBuffers window
              GL.flush
              GL.finish
              G.pollEvents
              G.windowShouldClose window
          unless close $ go
    go
      `finally`
      liftIO
      ( do
          G.destroyWindow window
          G.terminate
          exitSuccess
      )

openGLConsumer'
  :: (MonadSafe m, MonadIO m)
  => DIM2
  -> Consumer' (Array DIM2 (Word8,Word8,Word8)) m ()
openGLConsumer' (Z :. height :. width) =
  do
    let s = Size (P.fromIntegral width) (P.fromIntegral height)
    window <- liftIO $ do
      _b <- G.init
      mw@(Just window) <-
        G.createWindow width height "something" Nothing Nothing
      G.makeContextCurrent mw
      G.setCursorInputMode window $ G.CursorInputMode'Hidden
      G.setKeyCallback window $ Just keyCallback
      GL.clearColor $= Color4 0 0 0 0
      GL.shadeModel $= Flat
      GL.rowAlignment Unpack $= 1
      return window
    let
      go =
        do
          v <- process' <$> await
          close <- liftIO $
            do
              VS.unsafeWith v (drawPixels s . toData)
              G.pollEvents
              G.swapBuffers window
              G.windowShouldClose window
          unless close $ go
    go
      `finally`
      liftIO
      ( do
          G.destroyWindow window
          G.terminate
          exitSuccess
      )

keyCallback :: G.KeyCallback
keyCallback window key _scancode action _mods =
  when (key P.== G.Key'Escape P.&& action P.== G.KeyState'Pressed) $
  G.setWindowShouldClose window True

process :: Size -> Array DIM2 Word32 -> IO ()
process s arr =
  let
    v = toVectors arr :: VS.Vector Word32
    v' = VS.map (\p -> let (r,g,b,_a) = unpack8 p in toColor r g b) v
  in
    VS.unsafeWith v' (drawPixels s . toData)

process' :: Array DIM2 (Word8,Word8,Word8) -> VS.Vector (Color3 GLubyte)
process' arr =
  let ((((),bs),gs),rs) = toVectors arr
  in VS.zipWith3 toColor rs gs bs

toData :: Ptr (Color3 GLubyte) -> PixelData (Color3 GLubyte)
toData = PixelData BGR UnsignedByte

toColor :: Word8 -> Word8 -> Word8 -> Color3 GLubyte
toColor r g b =
  let
    r' = P.fromIntegral r
    g' = P.fromIntegral g
    b' = P.fromIntegral b
  in Color3 b' g' r'

pack8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
pack8 x y z w =
  P.fromIntegral w `B.shiftL` 24 .|.
  P.fromIntegral z `B.shiftL` 16 .|.
  P.fromIntegral y `B.shiftL` 8 .|.
  P.fromIntegral x

packRGBTupA :: Exp (Word8,Word8,Word8) -> Exp Word32
packRGBTupA e =
  let
    (r,g,b) = unlift e :: (Exp Word8, Exp Word8, Exp Word8)
    a = 255 :: Exp Word8
  in
    A.fromIntegral a `A.shiftL` 24 A..|.
    A.fromIntegral b `A.shiftL` 16 A..|.
    A.fromIntegral g `A.shiftL` 8 A..|.
    A.fromIntegral r

packRGBVecA :: Exp (V3 Word8) -> Exp Word32
packRGBVecA e =
  let
    (V3 r g b) = unlift e
    a = 255 :: Exp Word8
  in
    A.fromIntegral a `A.shiftL` 24 A..|.
    A.fromIntegral b `A.shiftL` 16 A..|.
    A.fromIntegral g `A.shiftL` 8 A..|.
    A.fromIntegral r

unpack8 :: Word32 -> (Word8, Word8, Word8, Word8)
unpack8 xyzw =
  let w = P.fromIntegral (xyzw `B.shiftR` 24)
      z = P.fromIntegral (xyzw `B.shiftR` 16)
      y = P.fromIntegral (xyzw `B.shiftR` 8)
      x = P.fromIntegral xyzw
  in
    (x,y,z,w)

unpackRGBVec :: Word32 -> V3 Word8
unpackRGBVec xyzw =
  let _w = P.fromIntegral (xyzw `B.shiftR` 24) :: Word32
      z = P.fromIntegral (xyzw `B.shiftR` 16)
      y = P.fromIntegral (xyzw `B.shiftR` 8)
      x = P.fromIntegral xyzw
  in
    V3 x y z
