{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
module GameLoop where
import RenderState (BoardInfo, RenderState(..), HasRenderState (..), HasBoardInfo (..), RenderMessage, updateRenderState)
import qualified RenderState
import GameState (GameState, HasGameState (..), Event, move)
import EventQueue (EventQueue, setSpeed, readEvent)
import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader, asks)
import Control.Monad.State (evalStateT, StateT, MonadState, gets)
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Foldable (forM_)

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

class Monad m => MonadQueue m where
  pullEvent :: m Event

class Monad m => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class Monad m => MonadRender m where
  render :: m ()

instance (Monad m, MonadIO m) => MonadQueue (App m) where
  pullEvent = asks getEventQueue >>= liftIO . readEvent

instance (Monad m) => MonadSnake (App m) where
  updateGameState = move

  updateRenderState :: [RenderMessage] -> App m ()
  updateRenderState rms = do
    forM_ rms $ \rm -> RenderState.updateRenderState rm

instance (Monad m, MonadIO m) => MonadRender (App m) where
  render = RenderState.render

setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = do
  event_queue <- asks getEventQueue
  s <- gets (score . getRenderState)
  liftIO $ setSpeed s event_queue

gameStep :: (MonadQueue m, MonadSnake m, MonadRender m) => m ()
gameStep = pullEvent >>= updateGameState >>= GameLoop.updateRenderState >> render

gameloop :: ( MonadQueue m
            , MonadSnake m
            , MonadRender m
            , MonadState state m
            , HasRenderState state
            , MonadReader env m
            , HasEventQueue env
            , MonadIO m )
         => m ()
gameloop = do
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep
  isGameOver <- gets (gameOver . getRenderState)
  unless isGameOver gameloop

-- gameStep :: (MonadReader env m, HasBoardInfo env, HasEventQueue env, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => m ()
-- gameStep = void $ asks getEventQueue >>= liftIO . readEvent >>= move >>= render

-- gameloop :: (MonadReader env m, HasBoardInfo env, HasEventQueue env, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => m ()
-- gameloop = do
--   queue <- asks getEventQueue
--   s <- gets (score . getRenderState)
--   newSpeed <- liftIO $ setSpeed s queue
--   liftIO $ threadDelay newSpeed
--   gameStep
--   game_over <- gets (gameOver . getRenderState)
--   unless game_over gameloop

run :: Env -> AppState -> IO ()
run env state = runApp gameloop `runReaderT` env `evalStateT` state
