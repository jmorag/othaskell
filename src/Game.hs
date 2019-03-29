{-|
Module          : Game
Description     : Top level of the Game module. Runs the game and displays turns
-}

module Game where

import           Game.Prelude
import           Game.Logic
import           Game.Strategies
import           Game.Render
import           Graphics.Gloss.Interface.IO.Simulate
                                         hiding ( black
                                                , white
                                                )

import           System.Timeout

data Config = Config { black :: Strategy
                     , white :: Strategy
                     , initialBoard :: Gamestate
                     }

data Gameover = Finished | Timeout

stepGame :: Gamestate -> ReaderT Config IO (Either Gameover Gamestate)
stepGame gs = do
  config <- ask
  next   <-
    liftIO
    .   timeout (10000000 :: Int)
    $   return
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

runGame :: Config -> IO ()
runGame config =
  simulateIO (InWindow "game" (800, 800) (0, 0))
             (dark . dark $ green)
             1
             (initialBoard config)
             (return . renderGloss)
    $ \_ _ gs -> do
        next <- runReaderT (stepGame gs) config
        case next of
          Right gs'      -> return gs'
          Left  Finished -> do
            putText
              $  "Game over: Black :: "
              <> show (fst (score gs))
              <> ", White :: "
              <> show (snd (score gs))
            exitSuccess
          Left Timeout -> do
            putText $ "Player " <> show (_player gs) <> " timed out"
            exitSuccess
