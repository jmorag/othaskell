{-|
Module      : Game.Render
Description : Render the game visually
-}
module Game.Render
  ( renderState
  )
where

import           Game.Prelude
import           Game.Logic
import qualified Data.Text                     as T

import           Graphics.Gloss

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
