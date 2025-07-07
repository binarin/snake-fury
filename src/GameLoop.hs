{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameLoop where
import RenderState (BoardInfo, RenderState(..), render)
import GameState (GameState, move)
import EventQueue (EventQueue, setSpeed, readEvent)
import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader)
import Control.Monad.State (runStateT, StateT, MonadState)
import Control.Monad.Identity (runIdentity)
import Data.ByteString.Builder
import System.IO (stdout)
import Control.Monad (unless)
import Control.Monad.IO.Class

data AppState = AppState GameState RenderState

newtype App m a = App { runApp :: ReaderT BoardInfo (StateT AppState m) a }
  deriving (Functor, Applicative, Monad, MonadReader BoardInfo, MonadState AppState, MonadIO)

-- The game loop is easy:
--   - wait some time
--   - read an Event from the queue
--   - Update the GameState
--   - Update the RenderState based on message delivered by GameState update
--   - Render into the console
gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop binf gstate rstate queue = do
  speed <- setSpeed (score rstate) queue
  threadDelay speed
  event <- readEvent queue
  (delta, gstate') <- runReaderT (runStateT (move event) gstate) binf
  let (rendered, rstate') = runIdentity $ render delta binf rstate
      isGameOver = gameOver rstate'
  putStr "\ESC[2J" --This cleans the console screen
  hPutBuilder stdout rendered
  unless isGameOver $ gameloop binf gstate' rstate' queue
