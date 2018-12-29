{-|
Module          : Game
Description     : Top level of the Game module. Runs the game and displays turns
-}

module Game where

import Game.Prelude
import Game.Logic
import Game.Render
import Game.Strategies

import System.Timeout
import qualified Data.Text.IO as T

import qualified Graphics.Gloss as G

aiVsAi :: Gamestate -> Strategy -> Strategy -> [Gamestate]
aiVsAi initial blackStrategy whiteStrategy =
  initial
    : unfold
        (\gs ->
          (case _player gs of
              White -> whiteStrategy
              Black -> blackStrategy
            )
            gs
        )
        initial
 where
  -- More specialized version of unfoldr
  unfold fun seed = case fun seed of
    Nothing     -> []
    Just result -> result : unfold fun result

data Config = Config { black :: Strategy
                     , white :: Strategy
                     , render :: Gamestate -> IO ()
                     , message :: Text -> IO ()
                     }

textConfig :: Strategy -> Strategy -> Config
textConfig b w = Config b w (T.putStrLn . renderText) T.putStrLn

glossConfig :: Strategy -> Strategy -> Config
glossConfig b w = Config
  b
  w
  ( G.display (G.InWindow "Nice Window" (800, 800) (0, 0))
              (G.dark $ G.dark G.green)
  . renderGloss
  )
  T.putStrLn

renderAiVsAi :: Gamestate -> ReaderT Config IO ()
renderAiVsAi gs = do
  config <- ask
  liftIO $ render config gs
  next <- stepGame gs
  case next of
    Right gs' -> renderAiVsAi gs'
    Left Finished ->
      liftIO
        $  message config
        $  "Game over: Black :: "
        <> show (fst (score gs))
        <> ", White :: "
        <> show (snd (score gs))

    Left Timeout ->
      liftIO $ message config $ "Player " <> show (_player gs) <> " timed out"

data Gameover = Finished | Timeout

stepGame :: Gamestate -> ReaderT Config IO (Either Gameover Gamestate)
stepGame gs = do
  config <- ask
  next   <-
    liftIO
    .  timeout (10000000 :: Int)
    $  return
    $!! (case _player gs of
         White -> white config
         Black -> black config
       )
         gs
  case next of
    Nothing         -> return $ Left Timeout
    Just Nothing    -> return $ Left Finished
    Just (Just gs') -> return $ Right gs'

-- | If the game is over, just keeps returning the same state
simpleStep :: Gamestate -> Reader Config Gamestate
simpleStep gs = do
  config <- ask
  let next =
        (case _player gs of
            White -> white config
            Black -> black config
          )
          gs
  return $ fromMaybe gs next
