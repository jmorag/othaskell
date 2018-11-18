{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
  -- ( someFunc
  -- , initialBoard
  -- , initialState
  -- , difficulty
  -- ) 
  where

import Lib.Prelude

-- | Prints an initial small othello board
someFunc :: IO ()
someFunc = print (initialState (mkDifficulty 4))

data Player
  = White
  | Black
  deriving (Show, Eq)

-- | A square can either be occupied by a player or empty
type Square = Maybe Player

-- | Matrix index of where on the board we are
type Position = (Int, Int)

-- | Board uses matrix indexing convention
--
-- Top left of board with side-length n is (1,1) and bottom right is (n,n).
type Board = Array Position Square

-- | Defines the difficulty in terms of board size
--
-- Difficulty of 1 -> 2x2 board, 2 -> 4x4 board, etc.
-- Othello board sizes must always be even and this newtype can ensure that
newtype Difficulty =
  Difficulty Int

-- | Smart constructor for difficulty of game ensuring valid board dimensions
mkDifficulty :: Int -> Difficulty
mkDifficulty i = Difficulty (2 * i)

-- | Create an initial board given dimentions
initialBoard :: Difficulty -> Board
initialBoard (Difficulty side) =
  let mkval (row, col)
        | (row, col) == (side `div` 2, side `div` 2) = Just White
        | (row, col) == (side `div` 2 + 1, side `div` 2 + 1) = Just White
        | (row, col) == (side `div` 2, side `div` 2 + 1) = Just Black
        | (row, col) == (side `div` 2 + 1, side `div` 2) = Just Black
        | otherwise = Nothing
   in array
        ((1, 1), (side, side))
        [ ((row, col), val)
        | row <- [1 .. side]
        , col <- [1 .. side]
        , let val = mkval (row, col)
        ]

-- | Gamestate to be extended later if needed
data Gamestate = Gamestate
  { _board :: Board
  , _player :: Player
  }
  deriving (Show, Eq)

initialState :: Difficulty -> Gamestate
initialState difficulty = Gamestate (initialBoard difficulty) Black 

-- WIP
-- -- | Given a Gamestate, return all possible next legal moves
-- legalMoves :: Gamestate -> [Position]
-- legalMoves (Gamestate board player) = 
--   let currentPieces = map fst $ filter (\square -> snd square == Just player) (assocs board)
--   in currentPieces
