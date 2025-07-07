{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleContexts #-}
module GameLoop where
import RenderState (BoardInfo, RenderState(..), HasRenderState (..), render)
import GameState (GameState, move, HasGameState (..))
import EventQueue (EventQueue, setSpeed, readEvent)
import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader)
import Control.Monad.State (evalStateT, StateT, MonadState, gets)
import Control.Monad (unless, void)
import Control.Monad.IO.Class

data AppState = AppState GameState RenderState

newtype App m a = App { runApp :: ReaderT BoardInfo (StateT AppState m) a }
  deriving (Functor, Applicative, Monad, MonadReader BoardInfo, MonadState AppState, MonadIO)

instance HasGameState AppState where
  getGameState (AppState gs _) = gs
  setGameState (AppState _ rs) gs = AppState gs rs

instance HasRenderState AppState where
  getRenderState (AppState _ rs) = rs
  setRenderState (AppState gs _) rs = AppState gs rs


gameStep :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameStep queue = void $ liftIO (readEvent queue) >>= move >>= render

gameloop :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameloop queue = do
  s <- gets (score . getRenderState)
  newSpeed <- liftIO $ setSpeed s queue
  liftIO $ threadDelay newSpeed
  gameStep queue
  game_over <- gets (gameOver . getRenderState)
  unless game_over $ gameloop queue

run :: BoardInfo -> AppState -> EventQueue -> IO ()
run binf app queue = gameloop queue `evalStateT` app `runReaderT` binf
