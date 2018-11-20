{-|
Module          : Game
Description     : Top level of the Game module. Runs the game and displays turns
-}

module Game
  (
  stupidGame
  )
where

import Game.Prelude
import Game.Logic
import Game.Render
import Game.Strategies

aiVsAi :: Gamestate -> Strategy -> Strategy -> [Gamestate]
aiVsAi initial blackStrategy whiteStrategy =
  initial : unfold2 True blackStrategy whiteStrategy initial
 where
  unfold2 b f g seed = case (if b then f else g) seed of
    Nothing  -> []
    Just res -> res : unfold2 (not b) f g res

stupidGame :: IO ()
stupidGame = mapM_ (putText . renderState)
  $ aiVsAi hardGame trivialStrategy trivialStrategy
