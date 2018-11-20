{-|
Module      : Game.Logic
Description : Logic of the othello game
-}
module Game.Logic
  ( easyGame
  , hardGame
  , nextStates
  , Player(..)
  , opponent
  , Gamestate(..)
  )
where

import           Game.Prelude

data Player
  = White
  | Black
  deriving (Show, Eq)

opponent :: Player -> Player
opponent player = if player == White then Black else White

-- | A square can either be occupied by a player or empty
type Square = Maybe Player

-- | Matrix index of where on the board we are
type Position = (Int, Int)

-- | Board uses matrix indexing convention
--
-- Top left of board with side-length n is (1,1) and bottom right is (n,n).
type Board = Array Position Square

-- | Safe array lookup
(!?) :: Ix i => Array i a -> i -> Maybe a
arr !? ix = if inRange (bounds arr) ix then Just (arr ! ix) else Nothing


-- | Defines the difficulty in terms of board size
--
-- Difficulty of 1 -> 2x2 board, 2 -> 4x4 board, etc.
-- Othello board sizes must always be even and this newtype can ensure that
newtype Difficulty = Difficulty Int

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
  in  array
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

easyGame, hardGame :: Gamestate
easyGame = initialState (mkDifficulty 2)
hardGame = initialState (mkDifficulty 4)

data Direction = N | NE | E | SE | S | SW | W | NW

allDirs :: [Direction]
allDirs = [N, NE, E, SE, S, SW, W, NW]

-- | Move along the board in the direction given
inc :: Position -> Direction -> Position
inc (row, col) = \case
  N  -> (pred row, col)
  NE -> (pred row, succ col)
  E  -> (row, succ col)
  SE -> (succ row, succ col)
  S  -> (succ row, col)
  SW -> (succ row, pred col)
  W  -> (row, pred col)
  NW -> (pred row, pred col)


-- | Given a Gamestate, return all possible next legal moves
legalMoves :: Gamestate -> [Position]
legalMoves gstate =
  let empties = map fst $ filter (isNothing . snd) (assocs (_board gstate))
  in  filter (\pos -> captures gstate pos /= []) empties

captures :: Gamestate -> Position -> [Position]
captures (Gamestate board player) pos = concatMap
  (\dir -> go dir (inc pos dir) [])
  validDirs
 where
  validDirs = filter
    (\dir -> board !? inc pos dir == Just (Just (opponent player)))
    allDirs

  go :: Direction -> Position -> [Position] -> [Position]
  go dir current traversed
    | board !? current `elem` [Nothing, Just Nothing] = []
    | board !? current == Just (Just player) = traversed
    | board !? current == Just (Just (opponent player)) = go
      dir
      (inc current dir)
      (current : traversed)
    | otherwise = panic "Unreachable"

nextStates :: Gamestate -> [Gamestate]
nextStates gstate@(Gamestate board player) = map nextState (legalMoves gstate)
 where
  nextState pos =
    let newBoard =
          board
            // ( (pos, Just player)
               : zip (captures gstate pos) (repeat (Just player))
               )
    in  Gamestate newBoard (opponent player)
