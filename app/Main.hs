module Main where

import Protolude
import Game
import Game.Render
import Game.Logic
import Game.Strategies
import Graphics.Gloss.Interface.IO.Simulate


main :: IO ()
main = simulateIO
  (InWindow "game" (800, 800) (0, 0))
  (dark $ dark green)
  1
  hardGame
  (return . renderGloss)
  (\_ _ gs -> do
    next <- runReaderT (stepGame gs)
                       (glossConfig negamaxStrategy trivialStrategy)
    case next of
      Right gs'      -> return gs'
      Left  Finished -> do
        putText
          $  "Game over: Black :: "
          <> show (fst (score gs))
          <> ", White :: "
          <> show (snd (score gs))
        return gs
      Left Timeout -> do
        putText $ "Player " <> show (_player gs) <> " timed out"
        return gs
  )
