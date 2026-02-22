{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Onitama
  ( give5Cards,
    GameState (..),
    Piece (..),
    PieceKind (..),
    Move (..),
    applyMove,
    initGameState,
    cardMoves,
    inBounds,
    legalMoves,
    formatMove,
    oppositeColor,
    replayGame,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import GHC.Generics (Generic)
import Data.Bifunctor (bimap, first, second)
import Data.Char (isAlpha, isDigit, toLower, toUpper)
import Data.Foldable (foldrM)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Types (Card, CardSet (..), Color (..), MoveNotation)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data PieceKind = King | Pawn deriving (Eq, Show)

data Piece = Piece
  { pieceColor :: Color,
    pieceKind :: PieceKind,
    piecePos :: (Int, Int)
  }
  deriving (Eq, Show)

data Move = Move
  { moveColor :: Color,
    moveFrom :: (Int, Int),
    moveTo :: (Int, Int),
    moveCard :: String
  }
  deriving (Eq, Show, Generic, NFData)

data GameState = GameState
  { gsBoard :: [Piece],
    gsWhiteCards :: (String, String),
    gsBlackCards :: (String, String),
    gsCommonCard :: String,
    gsNextColor :: Color,
    gsWinner :: Maybe Color
  }
  deriving (Eq, Show)

-- | Single source of truth for all card definitions.
-- (lowercase name, movement vectors from White's perspective, starting player)
cardDefs :: [(String, [(Int, Int)], Color)]
cardDefs = baseCardDefs ++ expansionCardDefs

-- | Base game cards (16 cards).
baseCardDefs :: [(String, [(Int, Int)], Color)]
baseCardDefs =
  [ ("boar", [(-1, 0), (1, 0), (0, 1)], White),
    ("cobra", [(1, 1), (1, -1), (-1, 0)], White),
    ("crab", [(-2, 0), (2, 0), (0, 1)], Black),
    ("crane", [(0, 1), (-1, -1), (1, -1)], Black),
    ("dragon", [(-2, 1), (-1, -1), (2, 1), (1, -1)], White),
    ("eel", [(-1, 1), (-1, -1), (1, 0)], Black),
    ("elephant", [(-1, 0), (-1, 1), (1, 0), (1, 1)], White),
    ("frog", [(-2, 0), (-1, 1), (1, -1)], White),
    ("goose", [(-1, 0), (-1, 1), (1, 0), (1, -1)], Black),
    ("horse", [(-1, 0), (0, 1), (0, -1)], White),
    ("mantis", [(-1, 1), (1, 1), (0, -1)], White),
    ("monkey", [(-1, 1), (-1, -1), (1, 1), (1, -1)], Black),
    ("ox", [(1, 0), (0, 1), (0, -1)], Black),
    ("rabbit", [(2, 0), (1, 1), (-1, -1)], Black),
    ("rooster", [(-1, 0), (-1, -1), (1, 0), (1, 1)], White),
    ("tiger", [(0, 2), (0, -1)], Black)
  ]

-- | Sensei's Path expansion cards (16 cards).
expansionCardDefs :: [(String, [(Int, Int)], Color)]
expansionCardDefs =
  [ ("bear", [(-1, 1), (0, 1), (1, -1)], Black),
    ("dog", [(-1, 1), (-1, 0), (-1, -1)], Black),
    ("fox", [(1, 1), (1, 0), (1, -1)], White),
    ("giraffe", [(-2, 1), (0, -1), (2, 1)], Black),
    ("iguana", [(-2, 1), (0, 1), (1, -1)], White),
    ("kirin", [(-1, 2), (0, -2), (1, 2)], White),
    ("mouse", [(-1, -1), (0, 1), (1, 0)], Black),
    ("otter", [(-1, 1), (1, -1), (2, 0)], White),
    ("panda", [(-1, -1), (0, 1), (1, 1)], White),
    ("phoenix", [(-2, 0), (-1, 1), (1, 1), (2, 0)], Black),
    ("rat", [(-1, 0), (0, 1), (1, -1)], White),
    ("sable", [(-2, 0), (-1, -1), (1, 1)], Black),
    ("sea snake", [(-1, -1), (0, 1), (2, 0)], Black),
    ("tanuki", [(-1, -1), (0, 1), (2, 1)], Black),
    ("turtle", [(-2, 0), (-1, -1), (1, -1), (2, 0)], White),
    ("viper", [(-2, 0), (0, 1), (1, -1)], White)
  ]

-- | Card name → movement vectors lookup map.
cardMoves :: Map.Map String [(Int, Int)]
cardMoves = Map.fromList [(name, moves) | (name, moves, _) <- cardDefs]

-- | Which color starts when this card is the common card.
cardStartPlayer :: String -> Color
cardStartPlayer name =
  maybe White (\(_, _, c) -> c) $ find (\(n, _, _) -> n == map toLower name) cardDefs

-- | Parse a move string like "w:c1c3:tiger" into a Move
parseMove :: MoveNotation -> Maybe Move
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
      Just $ Move {moveColor = color, moveFrom = from, moveTo = to, moveCard = map toLower cardStr}
    _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn c s = case rest of
  [] -> [chunk]
  _ : rest' -> chunk : splitOn c rest'
  where
    (chunk, rest) = break (== c) s

-- | Parse chess notation like "c3" into board coordinates.
chessToPos :: String -> Maybe (Int, Int)
chessToPos [col, row]
  | isAlpha col && isDigit row =
      let x = fromEnum (toLower col) - fromEnum 'a'
          y = fromEnum row - fromEnum '1'
       in guard (x >= 0 && x <= 4 && y >= 0 && y <= 4) >> Just (x, y)
chessToPos _ = Nothing

-- | Starting board: 5 white pieces on row 0, 5 black pieces on row 4, kings in the center.
initialBoard :: [Piece]
initialBoard =
  [ Piece {pieceColor = White, pieceKind = Pawn, piecePos = (0, 0)},
    Piece {pieceColor = White, pieceKind = Pawn, piecePos = (1, 0)},
    Piece {pieceColor = White, pieceKind = King, piecePos = (2, 0)},
    Piece {pieceColor = White, pieceKind = Pawn, piecePos = (3, 0)},
    Piece {pieceColor = White, pieceKind = Pawn, piecePos = (4, 0)},
    Piece {pieceColor = Black, pieceKind = Pawn, piecePos = (0, 4)},
    Piece {pieceColor = Black, pieceKind = Pawn, piecePos = (1, 4)},
    Piece {pieceColor = Black, pieceKind = King, piecePos = (2, 4)},
    Piece {pieceColor = Black, pieceKind = Pawn, piecePos = (3, 4)},
    Piece {pieceColor = Black, pieceKind = Pawn, piecePos = (4, 4)}
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

-- | Check if coordinates are within the 5×5 board.
inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x <= 4 && y >= 0 && y <= 4

-- | Apply a move to the game state. Returns Nothing if the move is illegal.
applyMove :: Move -> GameState -> Maybe GameState
applyMove pm gs = do
  guard (isNothing $ gsWinner gs)
  guard (moveColor pm == gsNextColor gs)

  let board = gsBoard gs
      from = moveFrom pm
      to = moveTo pm
      cardName = moveCard pm
      color = moveColor pm

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

      opponentColor = oppositeColor color
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

-- | Card names (title-cased) for a given card set.
validCards :: CardSet -> [Card]
validCards cardSet = [titleCase name | (name, _, _) <- defs]
  where
    defs = case cardSet of
      BaseOnly -> baseCardDefs
      WithExpansion -> cardDefs
    titleCase = unwords . map capitalize . words
    capitalize [] = []
    capitalize (c : cs) = toUpper c : cs

-- | Deal 5 random cards from the given card set.
give5Cards :: CardSet -> IO [Card]
give5Cards cardSet = do
  let pool = validCards cardSet
  rng <- newStdGen
  return . take 5 . shuffle' pool (length pool) $ rng

-- | Flip White ↔ Black.
oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

-- | Convert board coordinates to chess notation (e.g. (0,0) → "a1").
posToChess :: (Int, Int) -> String
posToChess (x, y) = [toEnum (fromEnum 'a' + x), toEnum (fromEnum '1' + y)]

-- | Serialize a Move to notation like "w:c1c3:tiger".
formatMove :: Move -> MoveNotation
formatMove pm =
  let colorStr = case moveColor pm of White -> "w"; Black -> "b"
   in colorStr ++ ":" ++ posToChess (moveFrom pm) ++ posToChess (moveTo pm) ++ ":" ++ moveCard pm

-- | Replay a full game from initial cards and move history. Returns Nothing if any move is invalid.
replayGame :: [Card] -> [MoveNotation] -> Maybe GameState
replayGame cs moves = mapM parseMove moves >>= foldrM applyMove (initGameState cs)

-- | Generate all legal moves for the current player.
legalMoves :: GameState -> [Move]
legalMoves gs =
  [ Move {moveColor = color, moveFrom = from, moveTo = to, moveCard = cardName}
    | let color = gsNextColor gs,
      let board = gsBoard gs,
      let friendlyPositions = [piecePos p | p <- board, pieceColor p == color],
      let (c1, c2) = case color of White -> gsWhiteCards gs; Black -> gsBlackCards gs,
      cardName <- [c1, c2],
      Just vectors <- [Map.lookup cardName cardMoves],
      piece <- board,
      pieceColor piece == color,
      let from = piecePos piece,
      vec <- vectors,
      let to = case color of
            White -> bimap (fst from +) (snd from +) vec
            Black -> bimap (fst from -) (snd from -) vec,
      inBounds to,
      to `notElem` friendlyPositions
  ]
