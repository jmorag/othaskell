module Main where

import Protolude
import Graphics.Gloss

main :: IO ()
main = display FullScreen white (Circle 80)
