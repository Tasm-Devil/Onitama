module Minimax (bestMove, bestMoveWithScore, evaluate, infinity) where

import Data.List (foldl')
import Types (Color (..))
import Onitama
  ( GameState (..),
    Move (..),
    Piece (..),
    PieceKind (..),
    applyMove,
    legalMoves,
  )

-- | Bound for the search window. Must be larger than any evaluation but far
-- away from 'minBound'/'maxBound': @negate minBound == minBound@ for 'Int',
-- which would silently corrupt the alpha-beta window.
infinity :: Int
infinity = 1000000

-- | Best move for the player to move, searching @depth@ plies.
bestMove :: Int -> GameState -> Maybe Move
bestMove depth = fmap fst . bestMoveWithScore depth

-- | Best move together with its negamax score from the mover's point of view.
bestMoveWithScore :: Int -> GameState -> Maybe (Move, Int)
bestMoveWithScore depth gs = case legalMoves gs of
  [] -> Nothing
  (m0 : ms) -> Just $ foldl' step (m0, scoreWithin (-infinity) m0) ms
  where
    -- Root alpha-beta: alpha is the best score found so far. A later move only
    -- replaces the current best if it is strictly better.
    step (best, alpha) m =
      let val = scoreWithin alpha m
       in if val > alpha then (m, val) else (best, alpha)
    scoreWithin alpha m = case applyMove m gs of
      Nothing -> -infinity
      Just gs' -> -negamax (depth - 1) (-infinity) (-alpha) gs'

negamax :: Int -> Int -> Int -> GameState -> Int
negamax depth alpha beta gs
  | depth == 0 || not (null (gsWinner gs)) = evaluate gs
  | null moves = evaluate gs
  | otherwise = go alpha moves
  where
    moves = legalMoves gs
    go a [] = a
    go a (m : ms) = case applyMove m gs of
      Nothing -> go a ms
      Just gs' ->
        let val = -negamax (depth - 1) (-beta) (-a) gs'
            a' = max a val
         in if a' >= beta then a' else go a' ms

-- | Static evaluation from the point of view of the player to move.
evaluate :: GameState -> Int
evaluate gs = case gsWinner gs of
  Just w
    | w == color -> 10000
    | otherwise -> -10000
  Nothing -> material + templeProximity
  where
    color = gsNextColor gs
    board = gsBoard gs
    friendlyPieces = filter (\p -> pieceColor p == color) board
    enemyPieces = filter (\p -> pieceColor p /= color) board
    material = 200 * (length friendlyPieces - length enemyPieces)
    enemyTemple = case color of White -> (2, 4); Black -> (2, 0)
    friendlyKingPos =
      case filter (\p -> pieceKind p == King) friendlyPieces of
        (k : _) -> piecePos k
        [] -> (2, 2)
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    templeProximity = (8 - dist friendlyKingPos enemyTemple) * 50
