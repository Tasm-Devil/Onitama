module Minimax (bestMove) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Game (Color (..))
import Onitama
  ( GameState (..),
    Move (..),
    Piece (..),
    PieceKind (..),
    applyMove,
    legalMoves,
  )

bestMove :: Int -> GameState -> Maybe Move
bestMove depth gs
  | null moves = Nothing
  | otherwise = Just $ maximumBy (comparing score) moves
  where
    moves = legalMoves gs
    score pm = case applyMove pm gs of
      Nothing -> minBound
      Just gs' -> -negamax (depth - 1) minBound maxBound gs'

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
