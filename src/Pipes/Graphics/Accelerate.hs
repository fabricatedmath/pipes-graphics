{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Graphics.Accelerate
  (imageToArray, arrayToImage, openGLConsumer, pack8, pack8A)
where

import Codec.Picture
import Codec.Picture.Types as J

import Control.Monad (unless)

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Data.Bits as A
import Data.Array.Accelerate.IO.Data.Vector.Storable
import Data.Bits as B
import qualified Data.Vector.Storable as VS

import Foreign.Ptr

import Graphics.Rendering.OpenGL as GL hiding (pixelMap)
import qualified Graphics.UI.GLFW as G

import Pipes
import Pipes.Safe

import Prelude as P

import System.Exit

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
  in Image ydim xdim . VS.unsafeCast $ v

openGLConsumer
  :: (MonadSafe m, MonadIO m)
  => DIM2
  -> Consumer' (Array DIM2 (Word8,Word8,Word8)) m ()
openGLConsumer (Z :. height :. width) =
  do
    let s = Size (P.fromIntegral width) (P.fromIntegral height)
    window <- liftIO $ do
      _b <- G.init
      mw@(Just window) <-
        G.createWindow width height "something" Nothing Nothing
      G.makeContextCurrent mw
      G.setCursorInputMode window $ G.CursorInputMode'Hidden
      --G.setKeyCallback window $ Just keyCallback
      GL.clearColor $= Color4 0 0 0 0
      GL.shadeModel $= Flat
      GL.rowAlignment Unpack $= 1
      return window
    let
      go =
        do
          v <- process <$> await
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
          print "killing"
          G.destroyWindow window
          G.terminate
          exitSuccess
      )

process :: Array DIM2 (Word8,Word8,Word8) -> VS.Vector (Color3 GLubyte)
process arr =
  let ((((),bs),gs),rs) = toVectors arr
  in VS.zipWith3 toColor bs gs rs

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

pack8A :: Exp (Word8,Word8,Word8) -> Exp Word32
pack8A e =
  let
    (r,g,b) = unlift e :: (Exp Word8, Exp Word8, Exp Word8)
    a = 255 :: Exp Word8
  in
    A.fromIntegral a `A.shiftL` 24 A..|.
    A.fromIntegral b `A.shiftL` 16 A..|.
    A.fromIntegral g `A.shiftL` 8 A..|.
    A.fromIntegral r

--word8OfFloat :: Exp Float -> Exp Word8
--word8OfFloat x = A.truncate (x * 255)

{-
unpack8 :: Exp Word32 -> Exp (Word8, Word8, Word8, Word8)
unpack8 xyzw =
  let w = A.fromIntegral (xyzw `A.shiftR` 24)
      z = A.fromIntegral (xyzw `A.shiftR` 16)
      y = A.fromIntegral (xyzw `A.shiftR` 8)
      x = A.fromIntegral xyzw
  in
    A.lift (x,y,z,w)
-}
