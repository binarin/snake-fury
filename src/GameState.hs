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

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

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

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation.
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> GameState -> (Point, GameState)
makeRandomPoint BoardInfo{height = h, width = w} gs@GameState{randomGen = gen} = ((x, y), gs {randomGen = g2})
  where
    (x, g1) = randomR (1, w) gen
    (y, g2) = randomR (1, h) g1

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
newApple :: BoardInfo -> GameState -> (Point, GameState)
newApple
  boardInfo
  gs@GameState{ snakeSeq = SnakeSeq{snakeHead = sHead, snakeBody = sBody}
              , applePosition = aPos
              }
  = go gs
  where
    occupied = aPos : sHead : F.toList sBody
    go gsSoFar = case makeRandomPoint boardInfo gsSoFar of
      (pt, gs') | pt `notElem` occupied -> (pt, gs')
      (_, gs') -> go gs'

{- |
>>> let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2)])
>>> let apple_pos = (2,2)
>>> let board_info = BoardInfo 2 2
>>> let game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
>>> fst $ newApple board_info game_state1
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

move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)
move
  bi@BoardInfo{}
  gs@GameState{applePosition = applePos}
  = (if appleEaten
     then [Board.RenderBoard ((applePos', Board.Apple):events), Board.IncreaseScore]
     else [Board.RenderBoard events]
    , gs2 {applePosition = applePos'}
    )

  where
    newHead = nextHead bi gs
    appleEaten = applePos == newHead
    (applePos', gs1) = if appleEaten then newApple bi gs else (applePos, gs)
    (events, gs2) = if appleEaten then extendSnake newHead bi gs else displaceSnake newHead bi gs1


extendSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)
extendSnake newHead _ gs@GameState{snakeSeq = SnakeSeq{snakeHead = oldHead, snakeBody = sb}}
  = ([(newHead, Board.SnakeHead), (oldHead, Board.Snake)],
     gs{snakeSeq = ss})
  where
    ss = SnakeSeq{snakeHead = newHead, snakeBody = oldHead :<| sb}

displaceSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)
displaceSnake newHead _ gs@GameState{snakeSeq = SnakeSeq{snakeHead = oldHead, snakeBody = S.Empty}}
  = ([(newHead, Board.SnakeHead), (oldHead, Board.Empty)],
     gs{snakeSeq = SnakeSeq{snakeHead = newHead, snakeBody = S.Empty}})
displaceSnake newHead _ gs@GameState{snakeSeq = SnakeSeq{snakeHead = oldHead, snakeBody = sbWithoutTail :|> oldTail}}
  = ([(newHead, Board.SnakeHead), (oldHead, Board.Snake), (oldTail, Board.Empty)]
    ,gs{snakeSeq = SnakeSeq{snakeHead = newHead, snakeBody = oldHead :<| sbWithoutTail}})

{-|
>>> let snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
>>> let apple_pos = (2,1)
>>> let board_info = BoardInfo 4 4

>>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
>>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
>>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
>>> fst $ move board_info game_state1
[RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]]

>>> fst $ move board_info game_state2
[RenderBoard [((2,4),Apple),((2,1),SnakeHead),((1,1),Snake)],IncreaseScore]

>>> fst $ move board_info game_state3
[RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]]

>>> let short_snake_seq = SnakeSeq (1,1) Data.Sequence.Empty
>>> let game_state4 = GameState short_snake_seq apple_pos West (System.Random.mkStdGen 1)
>>> let (events4, game_state4') = move board_info game_state4
>>> events4
[RenderBoard [((1,4),SnakeHead),((1,1),Empty)]]

-}

