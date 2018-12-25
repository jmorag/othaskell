{-|
Module      : Game.Strategies
Description : Various strategies used to play othello
-}
module Game.Strategies
  ( Strategy
  , trivialStrategy
  , greedyStrategy
  , negamaxStrategy
  )
where

import           Game.Prelude
import           Game.Logic
import           Data.Tree

-- | Given the current gamestate, pick a new one to play or give up
type Strategy = Gamestate -> Maybe Gamestate

trivialStrategy :: Strategy
trivialStrategy = head . nextStates

-- | Calculates the utility for of person who brought the game to the current state
utility :: Gamestate -> Int
utility gstate = case _player gstate of
  White -> uncurry (-) . score $ gstate
  Black -> uncurry (flip (-)) . score $ gstate

minimumBy', maximumBy' :: (a -> a -> Ordering) -> [a] -> Maybe a

minimumBy' _ [] = Nothing
minimumBy' f xs = Just $ minimumBy f xs

maximumBy' _ [] = Nothing
maximumBy' f xs = Just $ maximumBy f xs

-- | Minimize the opponent's utility for the next state
greedyStrategy :: Strategy
greedyStrategy = minimumBy' (compare `on` utility) . nextStates

fullGameTree :: Gamestate -> Tree Gamestate
fullGameTree = unfoldTree (\gs -> (gs, nextStates gs))

negamaxStrategy :: Strategy
negamaxStrategy gstate = rootLabel
  <$> maximumBy' (compare `on` negamaxVal) (subForest (fullGameTree gstate))
  where
    negamaxVal :: Tree Gamestate -> Int
    negamaxVal node = case subForest node of
      [] -> utility (rootLabel node)
      children -> negate $ maximum (map negamaxVal children)
