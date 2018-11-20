{-|
Module          : Game
Description     : Top level of the Game module. Runs the game and displays turns
-}

module Game
  (

  )
where

import Game.Prelude
import Game.Logic
import Game.Render
import Game.Strategies

runGame :: Gamestate -> Strategy -> Strategy -> [Gamestate]
runGame initial blackStrategy whiteStrategy =
  initial : unfold2 True blackStrategy whiteStrategy initial
 where
  unfold2 b f g seed = case (if b then f else g) seed of
    Nothing  -> []
    Just res -> res : unfold2 (not b) f g res

stupidGame :: IO ()
stupidGame = mapM_ (putText . renderState)
  $ runGame hardGame trivialStrategy trivialStrategy
