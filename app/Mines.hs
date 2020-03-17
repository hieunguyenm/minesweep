module Mines
  ( newMineField
  , renderGrid
  , size
  , reveal
  , hasWonOrLost
  , obfuscate
  , surroundingCells
  , allCoords
  , (??)
  , (//)
  , Grid
  , Coord
  , Boards
  , ObfuscatedGrid
  , Flagboard
  , Cell(..)
  , Difficulty(..)
  , Ending(..)
  )
where

import qualified Data.Sort                     as S
import qualified Data.HashSet                  as HS
import qualified System.Random                 as R
import qualified Data.HashMap.Lazy             as H
import qualified Control.Monad.State           as M

data Cell
  = Mine Hidden
  | Highlight Int Hidden
  | Empty Hidden
  deriving Show

data Ending = Win | Lose

data Difficulty = Easy | Medium | Hard

type Boards = (Grid, Flagboard, Ended)

type Coord = (Int, Int)

type Hidden = Bool

type Ended = Bool

type Grid = H.HashMap Coord Cell

type ObfuscatedGrid = H.HashMap Coord (Maybe Cell)

type Flagboard = HS.HashSet Coord

type Game = (Grid, Difficulty)

(??) = H.lookup

(//) :: H.HashMap k v
(//) = H.empty

-- newMineField creates new minefield with random mines, with numbers filled in.
newMineField :: M.StateT Game IO ()
newMineField = do
  newRandomMines
  setHighlights
  fillEmpty

-- size returns the board dimension.
size :: Difficulty -> Int
size d = fst $ parameters d

-- parameters gives the grid dimension + number of mines for a given difficulty.
parameters :: Difficulty -> (Int, Int)
parameters d = case d of
  Easy   -> (8, 10)
  Medium -> (16, 40)
  Hard   -> (24, 99)

-- fillEmpty fills in the remaining coordinates of a grid with empty cells.
fillEmpty :: M.StateT Game IO ()
fillEmpty = do
  (g, d) <- M.get
  M.put (fill (allCoords d) g, d)
 where
  fill :: [Coord] -> Grid -> Grid
  fill []       g = g
  fill (c : cs) g = case c ?? g of
    Nothing -> fill cs $ H.insert c (Empty True) g
    _       -> fill cs g

-- setHighlights fills in the surrounding numbers for each mine.
setHighlights :: M.StateT Game IO ()
setHighlights = do
  (g, d) <- M.get
  M.put (set (size d) g $ H.keys g, d)
 where
  set :: Int -> Grid -> [Coord] -> Grid
  set _ g [] = g
  set s g (c : cs) =
    flip (set s) cs
      $ H.unionWith overwrite g     -- merge old grid with new grid
      $ H.fromList                  -- create a new grid from mapped cells
      $ map (calc g)                -- calculate numbers surrounding cells
      $ surroundingCells s c        -- get surrounding cells

  -- calc calculates surrounding numbers for a mine.
  -- If a neighbouring cell already has a number then add 1.
  calc :: Grid -> Coord -> (Coord, Cell)
  calc g c = case c ?? g of
    Nothing              -> (c, Highlight 1 True)
    Just (Highlight x _) -> (c, Highlight (x + 1) True)
    Just (Mine b       ) -> (c, Mine b)

  -- overwrite prioritises the new/right grid when merging two grids.
  overwrite :: Cell -> Cell -> Cell
  overwrite a b = case (a, b) of
    (Mine b     , _            ) -> Mine b
    (Highlight{}, Highlight y _) -> Highlight y True

-- surroundingCells gives the coordinates of neighbouring cells.
surroundingCells :: Int -> Coord -> [Coord]
surroundingCells s (x, y) = filter
  (withinBounds s)
  [ (x    , y + 1)
  , (x    , y - 1)
  , (x - 1, y)
  , (x + 1, y)
  , (x - 1, y + 1)
  , (x + 1, y + 1)
  , (x - 1, y - 1)
  , (x + 1, y - 1)
  ]
 where
  withinBounds :: Int -> Coord -> Bool
  withinBounds s (x, y) = x >= 0 && x < s && y >= 0 && y < s

-- newRandomMines sets mines at random coordinates.
newRandomMines :: M.StateT Game IO ()
newRandomMines = do
  (g, d) <- M.get
  let (s, m) = parameters d
  g' <- M.lift $ set s m g
  M.put (g', d)
 where
  -- set places a mine at a random coordinate.
  set :: Int -> Int -> Grid -> IO Grid
  set _ 0 g = pure g
  set s m g = do
    c <- randomCoord s
    case c ?? g of
      Nothing -> set s (m - 1) $ H.insert c (Mine True) g
      _       -> set s m g

-- randomCoord generates a random coordinate within the bounds of the grid.
randomCoord :: Int -> IO Coord
randomCoord s = do
  g <- R.newStdGen
  let r = R.randomRs (0, s - 1) g
  pure (head r, head $ tail r)

-- renderGrid generates a string representing the grid.
renderGrid :: Difficulty -> Grid -> String
renderGrid d g =
  unlines                   -- merge each row into a single string
    $ map unwords           -- merge strings in each row
    $ (chunkify $ size d)   -- divide the list into rows
    $ map stringify         -- stringify every cell
    $ map (flip (??) g)     -- get cell at each coordinate
    $ allCoords d           -- take every coordinate in sorted order
 where
  -- chunkify divides a list of strings into sublists of a given size.
  chunkify :: Int -> [String] -> [[String]]
  chunkify s [] = []
  chunkify s l  = take s l : (chunkify s $ drop s l)
  stringify :: Maybe Cell -> String
  stringify (Just (Mine{}       )) = "X"
  stringify (Just (Highlight x _)) = show x
  stringify _                      = "_"

allCoords :: Difficulty -> [Coord]
allCoords d =
  [ (x, y) | y <- [0 .. ((size d) - 1)], x <- [0 .. ((size d) - 1)] ]

-- reveal reveals a given cell.
-- If an empty cell is clicked, all cells up to numbered cells are revealed.
reveal :: Int -> Coord -> Grid -> Grid
reveal i c g = case c ?? g of
  Just (Empty True) -> exposeSurrounding i H.empty g [c]
  Just x            -> exposeCell c g
  _                 -> g
 where
  undiscovered :: H.HashMap Coord Bool -> Coord -> Bool
  undiscovered e c = case H.lookup c e of
    Nothing -> True
    _       -> False
  exposeCell :: Coord -> Grid -> Grid
  exposeCell c g = case c ?? g of
    Just (Highlight x _) -> H.insert c (Highlight x False) g
    Just (Mine{}       ) -> H.insert c (Mine False) g
    Just (Empty{}      ) -> H.insert c (Empty False) g
    _                    -> g

  -- exposeSurrounding reveals empty neighbouring cells.
  exposeSurrounding :: Int -> H.HashMap Coord Bool -> Grid -> [Coord] -> Grid
  exposeSurrounding _    _    g []       = g
  exposeSurrounding size seen g (c : cs) = case c ?? g of
    Nothing       -> g
    -- Do not expose mines.
    Just (Mine{}) -> exposeSurrounding size (H.insert c True seen) g cs
    -- Expose highlights (numbered cells).
    Just (Highlight{}) ->
      exposeSurrounding size (H.insert c True seen) (exposeCell c g) cs
    -- Expose undiscovered neighbours of empty cells.
    _ ->
      exposeSurrounding size (H.insert c True seen) (exposeCell c g)
        $ (<>) cs
        $ filter (undiscovered seen)
        $ surroundingCells size c

-- hasWonOrLost checks for the following:
--   All mines flagged: win
--   Mine exposed:      lose
hasWonOrLost :: Grid -> Flagboard -> Maybe Ending
hasWonOrLost g f | mineCoords g == (S.sort $ HS.toList f) = Just Win
                 | or $ map (hasExposedMine g) $ mineCoords g = Just Lose
                 | otherwise                              = Nothing
 where
  hasExposedMine :: Grid -> Coord -> Bool
  hasExposedMine g c = case c ?? g of
    Just (Mine False) -> True
    _                 -> False

-- mineCoords gives the coordinates of mines.
mineCoords :: Grid -> [Coord]
mineCoords g = S.sort $ map fst $ filter isMine $ H.toList g
 where
  isMine :: (Coord, Cell) -> Bool
  isMine (_, Mine{}) = True
  isMine _           = False

-- obfuscate creates a new grid with hidden cells as Nothing.
obfuscate :: Grid -> ObfuscatedGrid
obfuscate g = H.fromList $ map obfs $ H.toList g
 where
  isHidden :: Cell -> Bool
  isHidden (Highlight _ True) = True
  isHidden (Mine  True      ) = True
  isHidden (Empty True      ) = True
  isHidden _                  = False

  -- obfs changes a cell to Nothing if it is hidden.
  obfs :: (Coord, Cell) -> (Coord, Maybe Cell)
  obfs (cd, cl) = if isHidden cl then (cd, Nothing) else (cd, Just cl)
