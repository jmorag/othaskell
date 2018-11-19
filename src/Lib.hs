{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
  ( someFunc
  , initialBoard
  , initialState
  , mkDifficulty
  , nextStates
  )
where

import qualified Data.Text                     as T
import           Lib.Prelude

-- | Prints an initial small othello board
someFunc :: IO ()
someFunc = print (initialState (mkDifficulty 4))

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

renderState :: Gamestate -> Text
renderState (Gamestate board player) =
  let (nrows, _) = snd (bounds board)
      getRow i =
        filter (\x -> let row = fst (fst x) in row == i) (assocs board)
      rows      = map getRow [1 .. nrows]
      renderRow = T.concat . map
        (\(_, square) -> case square of
          Nothing    -> " - " :: Text
          Just Black -> " B "
          Just White -> " W "
        )
  in  T.unlines $ show player : map renderRow rows

-- | Given the current gamestate, pick a new one to play or give up
type Strategy = Gamestate -> Maybe Gamestate

trivialStrategy :: Strategy
trivialStrategy = head . nextStates

-- Strange off by one error where the game terminates early in some cases
runGame :: Gamestate -> Strategy -> Strategy -> [Gamestate]
runGame initial blackStrategy whiteStrategy = initial : unfold2 True blackStrategy whiteStrategy initial
 where
  unfold2 b f' g' seed = case (if b then f' else g') seed of
    Nothing          -> []
    Just res -> res : unfold2 (not b) f' g' res

stupidGame :: IO ()
stupidGame = mapM_ (putText . renderState)
  $ runGame hardGame trivialStrategy (const Nothing)
