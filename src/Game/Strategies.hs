{-|
Module      : Game.Strategies
Description : Various strategies used to play othello
-}
module Game.Strategies
  ( Strategy
  , trivialStrategy
  , greedyStrategy
  , minimaxStrategy
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

minimaxTree :: Tree Gamestate -> Maybe Gamestate
minimaxTree tree =
  rootLabel
    <$> (case getPlayer tree of
          White -> minimumBy'
          Black -> maximumBy'
        )
          (compare `on` minimaxVal)
          (subForest tree)
 where
  minimaxVal subtree = case subForest subtree of
    [] -> utility (rootLabel subtree)
    forest ->
      (case getPlayer subtree of
          White -> minimum
          Black -> maximum
        )
        (map minimaxVal forest)

  getPlayer = _player . rootLabel


minimaxStrategy :: Strategy
minimaxStrategy = minimaxTree . fullGameTree

