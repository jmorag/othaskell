{-|
Module      : Game.Strategies
Description : Various strategies used to play othello
-}
module Game.Strategies
  ( Strategy
  , trivialStrategy
  , greedyStrategy
  )
where

import           Game.Prelude
import           Game.Logic
import           Data.Tree

-- | Given the current gamestate, pick a new one to play or give up
type Strategy = Gamestate -> Maybe Gamestate

trivialStrategy :: Strategy
trivialStrategy = head . nextStates

-- | Calculates the utility for whoever's turn it is in a state
utility :: Gamestate -> Int
utility gstate = case _player gstate of
  Black -> uncurry (-) . score $ gstate
  White -> uncurry (flip (-)) . score $ gstate

-- | Minimize the opponent's utility for the next state
greedyStrategy :: Strategy
greedyStrategy gstate = case nextStates gstate of
  []  -> Nothing
  gss -> Just $ minimumBy (compare `on` utility) gss


fullGameTree :: Gamestate -> Tree Gamestate
fullGameTree = unfoldTree (\gs -> (gs, nextStates gs))
