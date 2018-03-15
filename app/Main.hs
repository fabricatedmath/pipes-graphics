module Main where

import Pipes
import Pipes.Graphics
import Pipes.Safe

main :: IO ()
main =
  do
    let p = imageProducer 100 100
        ffmpegOpts = FFmpegOpts 1080 1080 60 "cat.mp4"
        c = ffmpegWriter ffmpegOpts
    runSafeT $ runEffect $ p >-> c
    runSafeT $ runEffect $ p >-> pngWriter 3 "/home/cdurham/Desktop/" "cats"
