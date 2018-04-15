{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Graphics.Accelerate
  ( imageToArray, arrayToImage, arrayToFlat, flatToImage
  , openGLConsumer, openGLConsumerFlat
  )
  -- , pack8, packRGBTupA, packRGBVecA)
where

import Codec.Picture
import Codec.Picture.Types as J

import Control.Monad (unless, when)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.IO.Data.Vector.Storable
import Data.Array.Accelerate.Linear.V3
import Data.Bits as B
import qualified Data.Vector.Storable as VS
import Data.Word.Word24

import Foreign.Ptr

import Graphics.Rendering.OpenGL as GL hiding (pixelMap)
import qualified Graphics.UI.GLFW as G

import Pipes
import Pipes.Safe

import Prelude as P

import System.Exit

--TODO: More robust frame buffers (get rid of drawPixels, which is deprecated)

imageToArray :: Image PixelRGB8 -> Array DIM2 (V3 Word8)
imageToArray img =
  let
    dim = Z :. J.imageHeight img :. imageWidth img
    rc = imageData $ pixelMap (\(PixelRGB8 r _g _b) -> r) img
    gc = imageData $ pixelMap (\(PixelRGB8 _r g _b) -> g) img
    bc = imageData $ pixelMap (\(PixelRGB8 _r _g b) -> b) img
  in
    fromVectors dim ((((),rc),gc),bc)

--flipVertically import Codec.Picture.Extra (flipVertically)
arrayToImage :: Array DIM2 (V3 Word8) -> Image PixelRGB8
arrayToImage arr =
  let
    (Z :. ydim :. xdim) = arrayShape arr
    ((((),rs),gs),bs) = toVectors arr
    rgbPacked = VS.zipWith3 (\r g b -> pack r g b) rs gs bs :: VS.Vector Word24
      where pack x y z =
              P.fromIntegral z `B.shiftL` 16 .|.
              P.fromIntegral y `B.shiftL` 8 .|.
              P.fromIntegral x
  in Image xdim ydim $ VS.unsafeCast rgbPacked

flatToImage :: DIM2 -> Array DIM1 Word8 -> Image PixelRGB8
flatToImage (Z :. ydim :. xdim) arr =
  let
    v = toVectors arr
  in Image xdim ydim v

arrayToFlat :: Acc (Array DIM2 (V3 Word8)) -> Acc (Array DIM1 Word8)
arrayToFlat arr =
  let
    arr' = flatten arr
    s = size arr * 3
    dim = index1 s
    f sh =
      let
        (i,r) = unindex1 sh `quotRem` 3
        V3 z y x = unlift $ arr' ! index1 i
      in
        caseof r [((A.== 0), z), ((A.== 1), y), ((A.== 2), x)] 0
  in
    A.generate dim f




--placeholder for idea quadrance squared distance of arrays
--map qd $ zipWith3 V3 rs gs bs

openGLConsumer
  :: (MonadSafe m, MonadIO m)
  => DIM2
  -> Consumer' (Array DIM2 (V3 Word8)) m ()
openGLConsumer (Z :. height :. width) =
  let
    keyCallback :: G.KeyCallback
    keyCallback window key _scancode action _mods =
      when (key P.== G.Key'Escape P.&& action P.== G.KeyState'Pressed) $
      G.setWindowShouldClose window True

    displayFrame :: Size -> Array DIM2 (V3 Word8) -> IO ()
    displayFrame s arr =
      let ((((),rs),gs),bs) = toVectors arr
          v = VS.zipWith3 toColor rs gs bs
      in
        VS.unsafeWith v (drawPixels s . toData)
      where
        toData :: Ptr (Color3 GLubyte) -> PixelData (Color3 GLubyte)
        toData = PixelData RGB UnsignedByte

        toColor :: Word8 -> Word8 -> Word8 -> Color3 GLubyte
        toColor r g b =
          let
            r' = P.fromIntegral r
            g' = P.fromIntegral g
            b' = P.fromIntegral b
          in Color3 b' g' r'
  in
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
          arr <- await
          close <- liftIO $
            do
              displayFrame s arr
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

openGLConsumerFlat
  :: (MonadSafe m, MonadIO m)
  => DIM2
  -> Consumer' (Array DIM1 Word8) m ()
openGLConsumerFlat (Z :. height :. width) =
  let
    keyCallback :: G.KeyCallback
    keyCallback window key _scancode action _mods =
      when (key P.== G.Key'Escape P.&& action P.== G.KeyState'Pressed) $
      G.setWindowShouldClose window True

    displayFrame :: Size -> Array DIM1 Word8 -> IO ()
    displayFrame s arr =
      let v' = toVectors arr
          v = VS.unsafeCast v'
      in
        VS.unsafeWith v (drawPixels s . toData)
      where
        toData :: Ptr (Color3 GLubyte) -> PixelData (Color3 GLubyte)
        toData = PixelData RGB UnsignedByte

        toColor :: Word8 -> Word8 -> Word8 -> Color3 GLubyte
        toColor r g b =
          let
            r' = P.fromIntegral r
            g' = P.fromIntegral g
            b' = P.fromIntegral b
          in Color3 b' g' r'
  in
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
          arr <- await
          close <- liftIO $
            do
              displayFrame s arr
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
