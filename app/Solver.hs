module Solver
  ( Action(..)
  , solve
  )
where

import           Mines                          ( ObfuscatedGrid
                                                , Flagboard
                                                , Coord
                                                , Cell(..)
                                                , surroundingCells
                                                , (??)
                                                )

import qualified Data.HashSet                  as HS
import qualified Data.HashMap.Lazy             as H

data Action
  = Mark Coord
  | Uncover Coord
  deriving Show

solve :: Int -> ObfuscatedGrid -> Flagboard -> Maybe Action
solve s o f = act s (revealedHighlights o) o f

act :: Int -> [(Coord, Cell)] -> ObfuscatedGrid -> Flagboard -> Maybe Action
act _ [] _ _ = Nothing
act s ((c, Highlight i _) : cs) o f
  |
  -- If the number of hidden neighbours is the difference between the highlight
  -- and the number of flagged neighbours, then the rest of the hidden 
  -- neighbours, if any, must be mines.
    (i - length fs == length hs && (not $ null hs)) = Just $ Mark $ head hs
  |
  -- If the number of flagged neighbours is equal to the highlight then the rest
  -- of the hidden neighbours, if any, must be safe.
    (i == length fs && (not $ null hs)) = Just $ Uncover $ head hs
  | otherwise                           = act s cs o f
 where
  (hs, fs) = neighbours s o f c
  -- neighbours gives a tuple of hidden neighbours and flagged neighbours.
  neighbours
    :: Int -> ObfuscatedGrid -> Flagboard -> Coord -> ([Coord], [Coord])
  neighbours s o f c = foldl (categorise o f) ([], []) $ surroundingCells s c
  categorise
    :: ObfuscatedGrid
    -> Flagboard
    -> ([Coord], [Coord])
    -> Coord
    -> ([Coord], [Coord])
  -- categorise sorts surrounding cells into hidden and flagged cells.
  categorise o f (hs, fs) c = case c ?? o of
    Just Nothing -> case HS.member c f of
      False -> (hs <> [c], fs)
      _     -> (hs, fs <> [c])
    _ -> (hs, fs)

-- revealedHighlights gives a list of all revealed numbered cells.
revealedHighlights :: ObfuscatedGrid -> [(Coord, Cell)]
revealedHighlights o = map unmaybe $ filter fh $ H.toList o
 where
  fh :: (Coord, Maybe Cell) -> Bool
  fh (_, Just (Highlight _ False)) = True
  fh _                             = False
  unmaybe :: (Coord, Maybe Cell) -> (Coord, Cell)
  unmaybe (cd, Just x) = (cd, x)
