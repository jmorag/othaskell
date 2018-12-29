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

renderGloss :: Gamestate -> Picture
renderGloss (Gamestate board player) =
  let (_, (_, n)) = bounds board
      offset      = translate (fi n * (-40)) (fi n * (-40))
  in  offset
      . pictures
      $ translate 0 600 (scale 0.25 0.25 $ text (show player <> "'s turn"))
      : map renderSquare (assocs board)
 where
  renderSquare ((i, j), p) = translate (80 * fi i) (80 * fi j) $ case p of
    Nothing    -> rectangleWire 80 80
    Just White -> pictures [rectangleWire 80 80, color white $ circleSolid 40]
    Just Black -> pictures [rectangleWire 80 80, color black $ circleSolid 40]

  fi = fromIntegral
