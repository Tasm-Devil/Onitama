{-# LANGUAGE OverloadedStrings #-}

module Onitama
  ( validateMove,
    MoveValidationError (..),
  )
where

import Control.Monad (guard)
import Data.Bifunctor (bimap, first, second)
import Data.Foldable (foldrM)
import Data.Char (isAlpha, isDigit, toLower)
import Data.List (find)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import Game (Card, Color (..), GameMove)

data PieceKind = King | Pawn deriving (Eq, Show)

data Piece = Piece
  { pieceColor :: Color,
    pieceKind :: PieceKind,
    piecePos :: (Int, Int)
  }
  deriving (Eq, Show)

data ParsedMove = ParsedMove
  { pmColor :: Color,
    pmFrom :: (Int, Int),
    pmTo :: (Int, Int),
    pmCard :: String
  }
  deriving (Eq, Show)

data GameState = GameState
  { gsBoard :: [Piece],
    gsWhiteCards :: (String, String),
    gsBlackCards :: (String, String),
    gsCommonCard :: String,
    gsNextColor :: Color,
    gsWinner :: Maybe Color
  }
  deriving (Eq, Show)

data MoveValidationError = InvalidMoveFormat | InvalidMove
  deriving (Eq, Show)

-- | Card movement vectors (from White's perspective).
-- Matches client/src/Game/Card.elm exactly.
cardMoves :: Map.Map String [(Int, Int)]
cardMoves =
  Map.fromList
    [ ("boar", [(-1, 0), (1, 0), (0, 1)]),
      ("cobra", [(1, 1), (1, -1), (-1, 0)]),
      ("crab", [(-2, 0), (2, 0), (0, 1)]),
      ("crane", [(0, 1), (-1, -1), (1, -1)]),
      ("dragon", [(-2, 1), (-1, -1), (2, 1), (1, -1)]),
      ("eel", [(-1, 1), (-1, -1), (1, 0)]),
      ("elephant", [(-1, 0), (-1, 1), (1, 0), (1, 1)]),
      ("frog", [(-2, 0), (-1, 1), (1, -1)]),
      ("goose", [(-1, 0), (-1, 1), (1, 0), (1, -1)]),
      ("horse", [(-1, 0), (0, 1), (0, -1)]),
      ("mantis", [(-1, 1), (1, 1), (0, -1)]),
      ("monkey", [(-1, 1), (-1, -1), (1, 1), (1, -1)]),
      ("ox", [(1, 0), (0, 1), (0, -1)]),
      ("rabbit", [(2, 0), (1, 1), (-1, -1)]),
      ("rooster", [(-1, 0), (-1, -1), (1, 0), (1, 1)]),
      ("tiger", [(0, 2), (0, -1)])
    ]

cardStartPlayer :: String -> Color
cardStartPlayer card = case map toLower card of
  "boar" -> White
  "cobra" -> White
  "crab" -> Black
  "crane" -> Black
  "dragon" -> White
  "eel" -> Black
  "elephant" -> White
  "frog" -> White
  "goose" -> Black
  "horse" -> White
  "mantis" -> White
  "monkey" -> Black
  "ox" -> Black
  "rabbit" -> Black
  "rooster" -> White
  "tiger" -> Black
  _ -> White

-- | Parse a move string like "w:c1c3:tiger" into a ParsedMove
parseMove :: GameMove -> Maybe ParsedMove
parseMove str =
  case splitOn ':' str of
    [colorStr, positions, cardStr] -> do
      color <- case colorStr of
        "w" -> Just White
        "b" -> Just Black
        _ -> Nothing
      guard (length positions == 4)
      let (fromStr, toStr) = splitAt 2 positions
      from <- chessToPos fromStr
      to <- chessToPos toStr
      Just $ ParsedMove color from to (map toLower cardStr)
    _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (c : cs)
  | c == sep = "" : splitOn sep cs
  | otherwise = let (h : t) = splitOn sep cs in (c : h) : t

chessToPos :: String -> Maybe (Int, Int)
chessToPos [col, row]
  | isAlpha col && isDigit row =
      let x = fromEnum (toLower col) - fromEnum 'a'
          y = fromEnum row - fromEnum '1'
       in guard (x >= 0 && x <= 4 && y >= 0 && y <= 4) >> Just (x, y)
chessToPos _ = Nothing

initialBoard :: [Piece]
initialBoard =
  [ Piece White Pawn (0, 0),
    Piece White Pawn (1, 0),
    Piece White King (2, 0),
    Piece White Pawn (3, 0),
    Piece White Pawn (4, 0),
    Piece Black Pawn (0, 4),
    Piece Black Pawn (1, 4),
    Piece Black King (2, 4),
    Piece Black Pawn (3, 4),
    Piece Black Pawn (4, 4)
  ]

-- | Initialize game state from dealt cards.
-- Cards order: [white1, white2, black1, black2, common]
initGameState :: [Card] -> GameState
initGameState cards =
  let lc = map toLower
      common = lc $ safeIndex cards 4 ""
   in GameState
        { gsBoard = initialBoard,
          gsWhiteCards = bimap lc lc (safeIndex cards 0 "", safeIndex cards 1 ""),
          gsBlackCards = bimap lc lc (safeIndex cards 2 "", safeIndex cards 3 ""),
          gsCommonCard = common,
          gsNextColor = cardStartPlayer common,
          gsWinner = Nothing
        }

safeIndex :: [a] -> Int -> a -> a
safeIndex xs i def
  | i < length xs = xs !! i
  | otherwise = def

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x <= 4 && y >= 0 && y <= 4

-- | Apply a parsed move to the game state
applyMove :: ParsedMove -> GameState -> Maybe GameState
applyMove pm gs = do
  guard (isNothing $ gsWinner gs)
  guard (pmColor pm == gsNextColor gs)

  let board = gsBoard gs
      from = pmFrom pm
      to = pmTo pm
      cardName = pmCard pm
      color = pmColor pm

  piece <- find (\p -> piecePos p == from) board
  guard (pieceColor piece == color)
  guard (inBounds to)
  guard (not $ any (\p -> piecePos p == to && pieceColor p == color) board)

  -- For Black, negate the vector (cards show moves from White's perspective)
  let delta = bimap (subtract $ fst from) (subtract $ snd from) to
      effectiveVector = case color of
        White -> delta
        Black -> bimap negate negate delta

  moves <- Map.lookup cardName cardMoves
  guard (effectiveVector `elem` moves)

  let playerCards = case color of
        White -> gsWhiteCards gs
        Black -> gsBlackCards gs
  guard (cardName == fst playerCards || cardName == snd playerCards)

  let newBoard =
        map (\p -> if piecePos p == from then p {piecePos = to} else p) $
          filter (\p -> not (piecePos p == to && pieceColor p /= color)) board

      common = gsCommonCard gs
      newPlayerCards =
        if cardName == fst playerCards
          then first (const common) playerCards
          else second (const common) playerCards

      (newWhiteCards, newBlackCards) = case color of
        White -> (newPlayerCards, gsBlackCards gs)
        Black -> (gsWhiteCards gs, newPlayerCards)

      opponentColor = case color of White -> Black; Black -> White
      opponentKingCaptured = not $ any (\p -> pieceColor p == opponentColor && pieceKind p == King) newBoard
      templePos = case color of
        White -> (2, 4)
        Black -> (2, 0)
      ourKingOnTemple = any (\p -> pieceColor p == color && pieceKind p == King && piecePos p == templePos) newBoard
      maybeWinner = if opponentKingCaptured || ourKingOnTemple then Just color else Nothing

  Just $
    GameState
      { gsBoard = newBoard,
        gsWhiteCards = newWhiteCards,
        gsBlackCards = newBlackCards,
        gsCommonCard = cardName,
        gsNextColor = opponentColor,
        gsWinner = maybeWinner
      }

-- | Validate moves by replaying the full history (prepend list, most recent first).
-- The new move should already be prepended by the caller.
validateMove :: [Card] -> [GameMove] -> Either MoveValidationError (Maybe Color)
validateMove cards historyMoves =
  case mapM parseMove historyMoves of
    Nothing -> Left InvalidMoveFormat
    Just parsedMoves ->
      case foldrM applyMove (initGameState cards) parsedMoves of
        Nothing -> Left InvalidMove
        Just finalState -> Right (gsWinner finalState)
