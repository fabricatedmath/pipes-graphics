module Main where

import Lib

import Pipes
import Pipes.Safe

main :: IO ()
main =
  do
    let p = imageProducer 100 100
        ffmpegOpts = FFmpegOpts 1080 1080 60 "cat.mp4"
        c = ffmpegConsumer ffmpegOpts
    runSafeT $ runEffect $ p >-> c
