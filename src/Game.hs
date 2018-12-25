{-|
Module          : Game
Description     : Top level of the Game module. Runs the game and displays turns
-}

module Game
where

import Game.Prelude
import Game.Logic
import Game.Render
import Game.Strategies

aiVsAi :: Gamestate -> Strategy -> Strategy -> [Gamestate]
aiVsAi initial blackStrategy whiteStrategy = initial : unfold
  (\gs -> (case _player gs of
    White -> whiteStrategy
    Black -> blackStrategy) gs
  )
  initial
 where
  -- More specialized version of unfoldr
  unfold fun seed = case fun seed of
    Nothing     -> []
    Just result -> result : unfold fun result

stupidGame :: IO ()
stupidGame = renderAiVsAi hardGame trivialStrategy trivialStrategy

renderAiVsAi :: Gamestate -> Strategy -> Strategy -> IO ()
renderAiVsAi gs blackS whiteS = do
  let gss = aiVsAi gs blackS whiteS
  mapM_ (putText . renderState) gss
  case score <$> lastMay gss of
    Nothing             -> return ()
    Just (black, white) -> do
      if black >= white
        then putText "Black wins!"
        else putText "White wins!"
      putText $ "Black :: " <> show black <> ", White :: " <> show white

smartGame :: IO ()
smartGame = renderAiVsAi easyGame negamaxStrategy negamaxStrategy
