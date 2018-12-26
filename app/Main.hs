module Main where

import Protolude
import Game.Render
import Graphics.Gloss
import Graphics.Gloss.Data.Picture


main :: IO ()
main = display (InWindow "Nice Window" (800, 800) (0, 0))
       white (emptyBoard 8 80)
