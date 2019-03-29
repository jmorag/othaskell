module Main where

import           Protolude
import           Game
import           Game.Logic
import           Game.Strategies


main :: IO ()
main = runGame $ Config
  { white        = trivialStrategy
  , black        = alphaBeta 8
  , initialBoard = hardGame
  }
