{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..) )
import qualified Data.Sequence as S
import System.Random ( StdGen, Random (randomR))
import Data.Maybe (isJust)
import qualified Data.Foldable as F
import Control.Monad.State.Strict (StateT, get, put, modify, gets, runState, MonadState)
import Control.Monad.Reader (ReaderT (runReaderT), ask, asks, local, MonadReader)
import Control.Monad.Trans.Class

-- $setup
-- >>> import Control.Monad.Identity


-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Movement

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)


-- type GameStep a = ReaderT BoardInfo (State GameState) a
newtype GameStep m a = GameStep { runGameStep :: ReaderT BoardInfo (StateT GameState m) a }

instance Functor m => Functor (GameStep m) where
  fmap f (GameStep r) = GameStep $ fmap f r

instance Monad m => Applicative (GameStep m) where
  pure :: a -> GameStep m a
  pure = GameStep . pure

  GameStep mf <*> GameStep ma = GameStep $ mf <*> ma

instance Monad m => Monad (GameStep m) where
  return = pure

  (>>=) :: GameStep m a -> (a -> GameStep m b) -> GameStep m b
  GameStep ma >>= f = GameStep (ma >>= runGameStep . f)

instance Monad m => MonadState GameState (GameStep m) where
  get :: GameStep m GameState
  get = GameStep (lift get)

  put :: GameState -> GameStep m ()
  put = GameStep . lift . put

instance Monad m => MonadReader BoardInfo (GameStep m) where
  ask :: GameStep m BoardInfo
  ask = GameStep ask

  local :: (BoardInfo -> BoardInfo) -> GameStep m a -> GameStep m a
  local f (GameStep rdr) = GameStep (local f rdr)


-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation.
--   Also, in the import list you have all relevant functions.

randomCoord :: (MonadState GameState m) => Int -> m Int
randomCoord maxN = do
  (n, gen) <- randomR (1, maxN) <$> gets randomGen
  modify (\s -> s { randomGen = gen })
  pure n

makeRandomPoint :: (MonadState GameState m, MonadReader BoardInfo m) => m Point
makeRandomPoint = do
  BoardInfo{height = h, width = w} <- ask
  (,) <$> randomCoord h <*> randomCoord w


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake pt SnakeSeq{snakeHead = hd, snakeBody = sq} = hd == pt || isJust (S.elemIndexL pt sq)

{-|
>>> let snake_seq = SnakeSeq (1,1) (S.fromList [(1,2), (1,3)])
>>> inSnake (1,1) snake_seq
True
>>> inSnake (1,2) snake_seq
True
>>> inSnake (1,4) snake_seq
False

-}

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead
  BoardInfo{height = h, width = w}
  GameState{movement = dir, snakeSeq = SnakeSeq{snakeHead = (y,x)}}
  = go dir
  where
    go North = (if y == 1 then h else y - 1, x)
    go South = (if y == h then 1 else y + 1, x)
    go West  = (y, if x == 1 then w else x - 1)
    go East  = (y, if x == w then 1 else x + 1)

-- |
-- >>> let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> let apple_pos = (2,2)
-- >>> let board_info = BoardInfo 4 4
-- >>> let game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> let game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> let game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1
-- (1,4)
-- >>> nextHead board_info game_state2
-- (2,1)
-- >>> nextHead board_info game_state3
-- (4,1)


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: (MonadState GameState m, MonadReader BoardInfo m) => m Point
newApple = do
  SnakeSeq{snakeHead = sHead, snakeBody = sBody} <- gets snakeSeq
  aPos <- gets applePosition
  let occupied = aPos : sHead : F.toList sBody
  pt <- makeRandomPoint
  if pt `elem` occupied
    then newApple
    else pure pt

{- |
>>> let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2)])
>>> let apple_pos = (2,2)
>>> let board_info = BoardInfo 2 2
>>> let game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
>>> fst $ runState (runReaderT newApple board_info) game_state1
(2,1)
-}

-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
--

-- move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)

step :: (MonadState GameState m, MonadReader BoardInfo m) => m [Board.RenderMessage]
step = do
  newHead <- asks nextHead <*> get
  appleEaten <- (== newHead) <$> gets applePosition
  if appleEaten
    then do
      ap <- newApple
      modify (\s -> s { applePosition = ap })
      events <- ((ap, Board.Apple):) <$> extendSnake newHead
      pure [Board.IncreaseScore, Board.RenderBoard events]
    else do
      (: []) . Board.RenderBoard <$> displaceSnake newHead

extendSnake :: MonadState GameState m => Point -> m RenderState.DeltaBoard
extendSnake newHead = do
  SnakeSeq{snakeHead = oldHead, snakeBody = sb} <- gets snakeSeq
  modify (\s -> s { snakeSeq = SnakeSeq{snakeHead = newHead, snakeBody = oldHead :<| sb} })
  pure [(newHead, Board.SnakeHead), (oldHead, Board.Snake)]

displaceSnake :: MonadState GameState m => Point -> m RenderState.DeltaBoard
displaceSnake newHead = gets snakeSeq >>= \case
  SnakeSeq{snakeHead = oldHead, snakeBody = S.Empty} -> do
    modify (\s -> s { snakeSeq = SnakeSeq{snakeHead = newHead, snakeBody = S.Empty} })
    pure [(newHead, Board.SnakeHead), (oldHead, Board.Empty)]
  SnakeSeq{snakeHead = oldHead, snakeBody = sbWithoutTail :|> oldTail} -> do
    modify (\s -> s { snakeSeq = SnakeSeq{snakeHead = newHead, snakeBody = oldHead :<| sbWithoutTail} })
    pure [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (oldTail, Board.Empty)]

{-|
>>> let evt = Tick
>>> let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
>>> let apple_pos = (2,1)
>>> let board_info = BoardInfo 4 4

>>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
>>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
>>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
>>> fst $ runIdentity $ move evt board_info game_state1
[RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]]

>>> fst $ runIdentity $ move evt board_info game_state2
[IncreaseScore,RenderBoard [((2,4),Apple),((2,1),SnakeHead),((1,1),Snake)]]

>>> fst $ runIdentity $ move evt board_info game_state3
[RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]]

>>> let short_snake_seq = SnakeSeq (1,1) Data.Sequence.Empty
>>> let game_state4 = GameState short_snake_seq apple_pos West (System.Random.mkStdGen 1)
>>> let (events4, game_state4') = runIdentity $ move evt board_info game_state4
>>> events4
[RenderBoard [((1,4),SnakeHead),((1,1),Empty)]]

-}

move :: Monad m => Event -> BoardInfo -> GameState -> m ([Board.RenderMessage], GameState)
move evt bi gs = do
  pure result
  where
    runAll = runState (runReaderT step bi)
    result = case evt of
      Tick -> runAll gs
      UserEvent m ->
        if movement gs == opositeMovement m
          then runAll gs
          else runAll $ gs {movement = m }
