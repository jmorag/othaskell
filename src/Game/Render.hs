{-|
Module      : Game.Render
Description : Render the game visually
-}
module Game.Render where

import           Game.Prelude
import           Game.Logic
import qualified Data.Text                     as T
import           Graphics.Gloss
-- import           Graphics.Gloss.Data.Picture

renderText :: Gamestate -> Text
renderText (Gamestate board player) =
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

-- | Draw an n x n empty othello board
emptyBoard :: Float -> Float -> Picture
emptyBoard n side = pictures $ map
    (\(x, y) -> translate x y (rectangleUpperWire side side)
    )
    [ (x, y) | x <- offsets, y <- offsets ]
    where offsets = take (floor n) (map (subtract ((n - 1) * side * 0.5)) [0, side..])
