{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Graphics.Accelerate
  ( arrayToFlat, flatToImage
  , openGLConsumerFlat
  )
where

import Codec.Picture

import Control.Monad (unless, when)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.IO.Data.Vector.Storable
import Data.Array.Accelerate.Linear.V3
import qualified Data.Vector.Storable as VS

import Foreign.Ptr

import Graphics.Rendering.OpenGL as GL hiding (pixelMap)
import qualified Graphics.UI.GLFW as G

import Pipes
import Pipes.Safe

import Prelude as P

import System.Exit

--TODO: More robust frame buffers (get rid of drawPixels, which is deprecated)

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
    displayFrame s arr = VS.unsafeWith v (drawPixels s . toData)
      where
        v = VS.unsafeCast $ toVectors arr

        toData :: Ptr (Color3 GLubyte) -> PixelData (Color3 GLubyte)
        toData = PixelData RGB UnsignedByte
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
