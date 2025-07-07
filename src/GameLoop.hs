{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleContexts #-}
module GameLoop where
import RenderState (BoardInfo, RenderState(..), HasRenderState (..), render, HasBoardInfo (..))
import GameState (GameState, move, HasGameState (..))
import EventQueue (EventQueue, setSpeed, readEvent)
import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader, asks)
import Control.Monad.State (evalStateT, StateT, MonadState, gets)
import Control.Monad (unless, void)
import Control.Monad.IO.Class

data AppState = AppState GameState RenderState
data Env = Env BoardInfo EventQueue

newtype App m a = App { runApp :: ReaderT Env (StateT AppState m) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState AppState, MonadIO)

instance HasGameState AppState where
  getGameState (AppState gs _) = gs
  setGameState (AppState _ rs) gs = AppState gs rs

instance HasRenderState AppState where
  getRenderState (AppState _ rs) = rs
  setRenderState (AppState gs _) rs = AppState gs rs

instance HasBoardInfo Env where
  getBoardInfo (Env bi _) = bi

class HasEventQueue env where
  getEventQueue :: env -> EventQueue

instance HasEventQueue Env where
  getEventQueue (Env _ eq) = eq

gameStep :: (MonadReader env m, HasBoardInfo env, HasEventQueue env, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => m ()
gameStep = void $ asks getEventQueue >>= liftIO . readEvent >>= move >>= render

gameloop :: (MonadReader env m, HasBoardInfo env, HasEventQueue env, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => m ()
gameloop = do
  queue <- asks getEventQueue
  s <- gets (score . getRenderState)
  newSpeed <- liftIO $ setSpeed s queue
  liftIO $ threadDelay newSpeed
  gameStep
  game_over <- gets (gameOver . getRenderState)
  unless game_over gameloop

run :: BoardInfo -> AppState -> EventQueue -> IO ()
run binf app queue = gameloop `evalStateT` app `runReaderT` Env binf queue
