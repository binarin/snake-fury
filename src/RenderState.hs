{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X


-}
module RenderState where

-- This are all imports you need. Feel free to import more things.
import Data.Array ( (//), listArray, Array, (!) )
import Data.ByteString.Builder
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.State.Strict (State, get, gets, runState, modify)
import Control.Monad.Trans (lift)
import Control.Monad (forM_)

-- A point is just a tuple of integers.
type Point = (Int, Int)

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType     -- ^The board is an Array indexed by points with elements of type CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage
  = RenderBoard DeltaBoard
  | GameOver
  | IncreaseScore
  deriving Show

-- | The RenderState contains the board and if the game is over or not.
data RenderState   = RenderState { board :: Board
                                 , gameOver :: Bool
                                 , score :: Int } deriving Show

type RenderStep a = ReaderT BoardInfo (State RenderState) a

-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid BoardInfo{height = h, width = w} =
  listArray ((1, 1), (h, w)) [ Empty | _ <- [1..h], _ <- [1..w] ]

{-|
>>> emptyGrid (BoardInfo 2 2)
array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]
-}

-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard bi snake apple = RenderState{ board = emptyGrid bi // [ (snake, SnakeHead), (apple, Apple) ]
                                              , gameOver = False
                                              , score = 0
                                              }

{-|
>>> buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False, score = 0}
-}


-- | Given tye current render state, and a message -> update the render state
updateRenderState :: RenderMessage -> RenderStep ()
updateRenderState GameOver = lift $ modify (\rs -> rs { gameOver = True })
updateRenderState (RenderBoard updates) = lift $ modify (\rs -> rs { board = board rs // updates })
updateRenderState IncreaseScore = do
  curScore <- lift $ gets score
  lift $ modify (\rs -> rs { score = curScore + 1 })


{-|
>>> let bi = BoardInfo 2 2
>>> render_state = buildInitialBoard bi (1,1) (2,2)
>>> message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), Empty)]
>>> snd $ runState (runReaderT (updateRenderState message1) bi) render_state
RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False, score = 0}

>>> message2 = GameOver
>>> snd $ runState (runReaderT (updateRenderState message2) bi) render_state
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True, score = 0}
-}


-- | Provisional Pretty printer
--   For each cell type choose a string to representing.
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some charachter followed by an space.
ppCell :: CellType -> Builder
ppCell Empty = stringUtf8 "- "
ppCell Snake = stringUtf8 "0 "
ppCell SnakeHead = stringUtf8 "$ "
ppCell Apple = stringUtf8 "X "

-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.
renderStep ::  [RenderMessage] -> RenderStep Builder
renderStep messages = do
  forM_ messages updateRenderState
  (w, h) <- (\bi -> (width bi, height bi)) <$> ask
  (brd, sc) <- (\rs -> (board rs, score rs)) <$> lift get
  let renderLine y = mconcat [ ppCell $ brd ! (y, x) | x <- [1..w] ] <> stringUtf8 "\n"
  let renderScore = stringUtf8 "*********\n" <> intDec sc <> "\n*********\n"
  pure $ renderScore <> mconcat [ renderLine y | y <- [1..h] ]

render :: [RenderMessage] -> BoardInfo -> RenderState -> (Builder, RenderState)
render ms bi rs = runState (runReaderT (renderStep ms) bi) rs


{- |
>>> let brd = listArray ((1,1), (3,4)) [Empty, Empty, Empty, Empty, Empty, Snake, SnakeHead, Empty, Empty, Empty, Empty, Apple]
>>> let board_info = BoardInfo 3 4
>>> let render_state = RenderState brd False 0
>>> let board_updates = [((3, 3), Apple), ((3, 4), Empty)]
>>> fst $ render [RenderBoard board_updates, IncreaseScore] board_info render_state
"*********\n1\n*********\n- - - - \n- 0 $ - \n- - X - \n"
-}
