module Main where

import Lib

import Pipes
import Pipes.Safe

main :: IO ()
main =
  do
    let p = imageProducer 100 100
        c = ffmpegConsumer 100 100
    runSafeT $ runEffect $ p >-> c
