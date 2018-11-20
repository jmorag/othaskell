{-|
Module      : Game.Strategies
Description : Various strategies used to play othello
-}
module Game.Strategies
  ( Strategy
  , trivialStrategy
  )
where

import Game.Prelude
import Game.Logic

-- | Given the current gamestate, pick a new one to play or give up
type Strategy = Gamestate -> Maybe Gamestate

trivialStrategy :: Strategy
trivialStrategy = head . nextStates

utility :: Player -> Gamestate -> Int
utility = \case
  Black -> uncurry (-) . score
  White -> uncurry (flip (-)) . score


