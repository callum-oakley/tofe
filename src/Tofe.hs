module Tofe
  ( Board
  , Column
  , Direction(..)
  , Score
  , State(..)
  , Tile
  , columns
  , initialState
  , move
  ) where

import Data.Array (Array, array, assocs, listArray, range, (!), (//))
import Data.List (group)
import Data.Maybe (isNothing, catMaybes)
import System.Random (randomRIO)

type Board = Array (Int, Int) Tile
type Column = [Tile]
type Index = (Int, Int)
type Score = Integer
type Tile = Maybe Integer
data Direction = North | East | South | West
data State = State { board :: Board, done :: Bool, score :: Score } deriving Eq

-- TODO paramaterise
boardSize :: Int
boardSize = 4

boardBounds :: (Index, Index)
boardBounds = ((0, 0), (boardSize - 1, boardSize - 1))

freeIndices :: Board -> [Index]
freeIndices = map fst . filter (isNothing . snd) . assocs

pickUniformly :: [a] -> IO a
pickUniformly xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i

checkIfDone :: State -> IO State
checkIfDone s = case freeIndices $ board s of
  [] -> return $ s { done = True }
  _ -> return s

placeTile :: State -> IO State
placeTile s = do
  i <- pickUniformly $ freeIndices $ board s
  -- We want a 1 in 10 chance of picking a 4
  tile <- pickUniformly $ 4 : replicate 9 2
  return $ s { board = board s // [(i, Just tile)] }

initialState :: IO State
initialState = placeTile State
  { board = listArray boardBounds $ repeat Nothing
  , done = False
  , score = 0
  }

r :: Direction -> Index -> Index
r North (x, y) = (x, boardSize - y - 1)
r East (x, y) = (y, boardSize - x - 1)
r South (x, y) = (x, y)
r West (x, y) = (y, x)

-- Rotate according to direction, such that pushing in that pushing tiles
-- downward is equivalent to pushing tiles in that direction.
rotate :: Direction -> Board -> Board
rotate d b = array boardBounds [(r d i, b ! i) | i <- range boardBounds]

unrotate :: Direction -> Board -> Board
unrotate d b = array boardBounds [(i, b ! r d i) | i <- range boardBounds]

precombine :: Column -> [[Integer]]
precombine = concatMap (reverse . chunksOf 2) . group . catMaybes
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Push all tiles to the bottom of the board, combining pairs of like tiles.
-- Points are scored for each combination.
applyGravity :: Board -> (Board, Score)
applyGravity b = (uncolumns cols, points)
  where
    cols = map (pad . map sum) precombinedCols
    points = sum $ concat $ filter ((> 1) . length) $ concat precombinedCols
    precombinedCols = map precombine $ columns b
    pad xs = replicate (boardSize - length xs) Nothing ++ map Just xs

-- TODO moves that don't change the board at all are not allowed
move :: Direction -> State -> IO State
move d s = if s /= s' then placeTile s' else checkIfDone s'
  where
    s' = s { board = unrotate d board', score = score s + score' }
    (board', score') = applyGravity $ rotate d $ board s

-- Represent a board as a list of columns.
columns :: Board -> [Column]
columns b =
  [ [ b ! (x, y) | y <- [boardSize - 1, boardSize - 2 .. 0]]
  | x <- [0 .. boardSize - 1]
  ]

uncolumns :: [Column] -> Board
uncolumns cols = array boardBounds
  [ ((x, boardSize - y - 1), cols !! x !! y)
  | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1]
  ]
