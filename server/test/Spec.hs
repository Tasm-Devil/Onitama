{-# LANGUAGE OverloadedStrings #-}

-- | Server test suite. Run with @make test-server@ (or @stack test@).
--
-- Covers move notation, rule enforcement, the "pass" rule for positions
-- without a legal move, the minimax search, and an end-to-end HTTP game
-- against the AI.
module Main (main) where

import Api (apiWithAssets)
import App (makeServer)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Value (..), decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Database
import Minimax (bestMove, bestMoveWithScore)
import qualified Minimax
import Network.HTTP.Types (Header, methodGet, methodPost)
import Network.Wai.Test (SResponse (..))
import Onitama
import Servant (serve)
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.Wai (WaiSession, request, with)
import Types

main :: IO ()
main = hspec $ do
  notationSpec
  rulesSpec
  passSpec
  minimaxSpec
  apiSpec

------------------------------------------------------------------------------
-- Helpers

piece :: Color -> PieceKind -> (Int, Int) -> Piece
piece c k pos = Piece {pieceColor = c, pieceKind = k, piecePos = pos}

allCardNames :: [String]
allCardNames = Map.keys cardMoves

baseCardNames :: [String]
baseCardNames =
  [ "boar", "cobra", "crab", "crane", "dragon", "eel", "elephant", "frog",
    "goose", "horse", "mantis", "monkey", "ox", "rabbit", "rooster", "tiger"
  ]

-- | Black has only its king left at @kingPos@ and must move with the given cards.
loneBlackKing :: (Int, Int) -> String -> String -> GameState
loneBlackKing kingPos c1 c2 =
  GameState
    { gsBoard = [piece Black King kingPos, piece White King (2, 4), piece White Pawn (0, 4)],
      gsWhiteCards = ("ox", "boar"),
      gsBlackCards = (c1, c2),
      gsCommonCard = "crab",
      gsNextColor = Black,
      gsWinner = Nothing
    }

-- | All positions with a lone Black king where Black cannot move a piece.
stuckPositions :: [String] -> [GameState]
stuckPositions names =
  [ gs
    | x <- [0 .. 4],
      y <- [0 .. 4],
      -- skip Black's goal temple and the squares occupied by White
      (x, y) `notElem` [(2, 0), (2, 4), (0, 4)],
      a <- names,
      b <- names,
      a < b,
      let gs = loneBlackKing (x, y) a b,
      null (stepMoves gs)
  ]

-- | Positions from a deterministic self-play game (White and Black search
-- with the given depths). Includes the start position, excludes the final one.
selfPlay :: Int -> CardSet -> Int -> Int -> Int -> [GameState]
selfPlay seed cardSet whiteDepth blackDepth maxPlies =
  go maxPlies (initGameState (dealCards cardSet (mkStdGen seed)))
  where
    go 0 _ = []
    go n gs
      | isJust (gsWinner gs) = []
      | otherwise =
          let depth = if gsNextColor gs == White then whiteDepth else blackDepth
           in case bestMove depth gs >>= (`applyMove` gs) of
                Nothing -> [gs]
                Just gs' -> gs : go (n - 1) gs'

-- | A sample of positions from several games, with base and expansion cards.
samplePositions :: [GameState]
samplePositions =
  concat [selfPlay seed cs 2 2 40 | seed <- [1 .. 6], cs <- [BaseOnly, WithExpansion]]

-- | Plain negamax without any pruning: the reference the real search must match.
plainNegamax :: Int -> GameState -> Int
plainNegamax depth gs
  | depth == 0 || isJust (gsWinner gs) = Minimax.evaluate gs
  | otherwise =
      case [-plainNegamax (depth - 1) gs' | m <- legalMoves gs, Just gs' <- [applyMove m gs]] of
        [] -> Minimax.evaluate gs
        vals -> maximum vals

-- | Every move string the current player could possibly type, brute force.
allCandidateMoves :: GameState -> [Move]
allCandidateMoves gs =
  [ Move {moveColor = gsNextColor gs, moveAction = action, moveCard = c}
    | let (c1, c2) = case gsNextColor gs of White -> gsWhiteCards gs; Black -> gsBlackCards gs,
      c <- nub [c1, c2],
      action <- Pass : [Step (fx, fy) (tx, ty) | fx <- [0 .. 4], fy <- [0 .. 4], tx <- [0 .. 4], ty <- [0 .. 4]]
  ]

-- | Black to move with Boar + Tiger on the given board.
blackToMove :: [Piece] -> GameState
blackToMove board =
  GameState
    { gsBoard = board,
      gsWhiteCards = ("ox", "crab"),
      gsBlackCards = ("boar", "tiger"),
      gsCommonCard = "eel",
      gsNextColor = Black,
      gsWinner = Nothing
    }

-- | Moves in a canonical order, for comparing move lists.
sortedNotation :: [Move] -> [MoveNotation]
sortedNotation = sort . map formatMove

------------------------------------------------------------------------------
-- Move notation

notationSpec :: Spec
notationSpec = describe "Move notation" $ do
  it "round-trips normal moves, passes and card names with spaces" $
    forM_ ["w:c1c3:tiger", "b:pass:fox", "b:a5a4:sea snake", "w:pass:sea snake"] $ \str ->
      fmap formatMove (parseMove str) `shouldBe` Just str

  it "parses a pass" $
    parseMove "b:pass:fox" `shouldBe` Just (Move Black Pass "fox")

  it "lowercases card names" $
    fmap moveCard (parseMove "w:c1c3:Tiger") `shouldBe` Just "tiger"

  it "rejects malformed strings" $
    forM_ ["x:a1a2:ox", "w:a1:ox", "w:a1a6:ox", "w:f1a2:ox", "w:pass", "w:skip:ox", ""] $ \str ->
      parseMove str `shouldBe` Nothing

------------------------------------------------------------------------------
-- Rules

-- | Cards in deal order: White 1, White 2, Black 1, Black 2, common.
-- Crab as common card means Black starts.
testCards :: [Card]
testCards = ["Ox", "Boar", "Bear", "Fox", "Crab"]

rulesSpec :: Spec
rulesSpec = describe "Rules" $ do
  it "lets the common card decide who starts" $
    fmap gsNextColor (replayGame testCards []) `shouldBe` Just Black

  it "accepts a legal move" $
    replayGame testCards ["b:a5a4:bear"] `shouldSatisfy` isJust

  it "rejects a move by the wrong player" $
    replayGame testCards ["w:a1a2:ox"] `shouldBe` Nothing

  it "rejects a card the player does not hold" $
    replayGame testCards ["b:a5a4:ox"] `shouldBe` Nothing

  it "rejects a move onto an own piece" $
    replayGame testCards ["b:a5b5:bear"] `shouldBe` Nothing

  -- Boar has (0,+1) for White, which becomes (0,-1) for Black: c2 -> c1.
  it "detects a win by capturing the king" $ do
    let gs = blackToMove [piece Black King (4, 4), piece Black Pawn (2, 1), piece White King (2, 0)]
    (applyMove (Move Black (Step (2, 1) (2, 0)) "boar") gs >>= gsWinner) `shouldBe` Just Black

  it "detects a win by reaching the opponent's temple" $ do
    let gs = blackToMove [piece Black King (2, 1), piece White King (4, 4)]
    (applyMove (Move Black (Step (2, 1) (2, 0)) "boar") gs >>= gsWinner) `shouldBe` Just Black

  it "does not end the game on an ordinary move" $ do
    let gs = blackToMove [piece Black King (4, 4), piece Black Pawn (2, 2), piece White King (0, 0)]
    fmap gsWinner (applyMove (Move Black (Step (2, 2) (2, 1)) "boar") gs) `shouldBe` Just Nothing

  it "refuses any move once the game is won" $ do
    let won = (blackToMove [piece Black King (4, 4), piece White King (0, 0)]) {gsWinner = Just White}
    legalMoves won `shouldBe` []
    applyMove (Move Black (Step (4, 4) (4, 3)) "boar") won `shouldBe` Nothing

  it "generates exactly the moves that applyMove accepts" $
    forM_ samplePositions $ \gs -> do
      let accepted = [m | m <- allCandidateMoves gs, isJust (applyMove m gs)]
      sortedNotation (legalMoves gs) `shouldBe` sortedNotation accepted

------------------------------------------------------------------------------
-- Passing (the bug where the AI got stuck)

passSpec :: Spec
passSpec = describe "Positions without a legal move" $ do
  let stuck = stuckPositions allCardNames

  it "exist with expansion cards (e.g. lone king on a1 with Bear + Fox)" $ do
    stuck `shouldSatisfy` (not . null)
    loneBlackKing (0, 0) "bear" "fox" `shouldSatisfy` (`elem` stuck)

  it "do not exist for a lone king with base cards only" $
    stuckPositions baseCardNames `shouldBe` []

  it "offer exactly one pass per card and nothing else" $
    forM_ stuck $ \gs -> do
      let (c1, c2) = gsBlackCards gs
      sortedNotation (legalMoves gs) `shouldBe` sortedNotation [Move Black Pass c1, Move Black Pass c2]

  it "let the AI pass instead of returning no move" $
    forM_ stuck $ \gs ->
      fmap moveAction (bestMove 5 gs) `shouldBe` Just Pass

  it "swap the card on a pass, keep the board and hand over the turn" $ do
    let gs = loneBlackKing (0, 0) "bear" "fox"
    case applyMove (Move Black Pass "bear") gs of
      Nothing -> expectationFailure "pass was rejected"
      Just gs' -> do
        gsBoard gs' `shouldBe` gsBoard gs
        gsBlackCards gs' `shouldBe` ("crab", "fox")
        gsCommonCard gs' `shouldBe` "bear"
        gsNextColor gs' `shouldBe` White
        gsWinner gs' `shouldBe` Nothing

  it "rejects a pass while a normal move exists" $ do
    replayGame testCards ["b:pass:bear"] `shouldBe` Nothing
    forM_ (take 50 samplePositions) $ \gs -> do
      let (c1, _) = case gsNextColor gs of White -> gsWhiteCards gs; Black -> gsBlackCards gs
      unless (null (stepMoves gs)) $
        applyMove (Move (gsNextColor gs) Pass c1) gs `shouldBe` Nothing

  it "rejects a pass with a card the player does not hold" $
    applyMove (Move Black Pass "ox") (loneBlackKing (0, 0) "bear" "fox") `shouldBe` Nothing

  it "are reached by the demo game for `make demo-pass` (White must pass)" $ do
    raw <- LBS.readFile "server/test/fixtures/must-pass.json"
    game <- case decode raw >>= Map.lookup 1 . Database.dbGames of
      Just g -> pure g
      Nothing -> ioError (userError "cannot read game 1 from must-pass.json")
    player_white game `shouldBe` Just (OidcUserId "dev")
    player_black game `shouldBe` Just (OidcUserId "__ai__")
    winner game `shouldBe` Nothing
    case replayGame (cards game) (map fst (history game)) of
      Nothing -> expectationFailure "demo game history does not replay"
      Just gs -> do
        gsNextColor gs `shouldBe` White
        stepMoves gs `shouldBe` []
        map moveAction (legalMoves gs) `shouldBe` [Pass, Pass]

------------------------------------------------------------------------------
-- Minimax

minimaxSpec :: Spec
minimaxSpec = describe "Minimax AI" $ do
  it "always returns a legal move in self-play positions" $
    forM_ samplePositions $ \gs ->
      case bestMove 3 gs of
        Nothing -> expectationFailure ("no move in " ++ show gs)
        Just m -> applyMove m gs `shouldSatisfy` isJust

  it "captures the king when it can win immediately" $ do
    -- Black pawn on c2 can take the White king on c1 with Boar:
    -- Boar has (0,+1) for White, which is (0,-1) for Black.
    let gs = blackToMove [piece Black King (4, 4), piece Black Pawn (2, 1), piece White King (2, 0), piece White Pawn (0, 0)]
    (bestMove 5 gs >>= (`applyMove` gs) >>= gsWinner) `shouldBe` Just Black

  it "matches plain negamax without pruning (alpha-beta window is sound)" $
    -- Regression test: the old search negated minBound, which overflows
    -- (negate minBound == minBound) and corrupted the alpha-beta window.
    forM_ (take 40 samplePositions) $ \gs ->
      fmap snd (bestMoveWithScore 3 gs)
        `shouldBe` Just (maximum [-plainNegamax 2 gs' | m <- legalMoves gs, Just gs' <- [applyMove m gs]])

  it "with depth 4 beats depth 2 in most seeded games" $ do
    let results = [playGame seed cs 2 4 | seed <- [1 .. 5], cs <- [BaseOnly, WithExpansion]]
        blackWins = length (filter (== Just Black) results)
    blackWins `shouldSatisfy` (>= 7)

-- | Play a full game (max 200 plies) and return the winner.
playGame :: Int -> CardSet -> Int -> Int -> Maybe Color
playGame seed cardSet whiteDepth blackDepth =
  let positions = selfPlay seed cardSet whiteDepth blackDepth 200
      final = last positions
   in case [gsWinner gs' | Just m <- [bestMove (depthFor final) final], Just gs' <- [applyMove m final]] of
        [w] -> w
        _ -> Nothing
  where
    depthFor gs = if gsNextColor gs == White then whiteDepth else blackDepth

------------------------------------------------------------------------------
-- HTTP API, end to end

authHeaders :: [Header]
authHeaders =
  [ ("X-Forwarded-User", "tester"),
    ("X-Forwarded-Preferred-Username", "Tester"),
    ("Content-Type", "application/json")
  ]

postJson :: LBS.ByteString -> LBS.ByteString -> WaiSession st SResponse
postJson path = request methodPost (LBS.toStrict path) authHeaders

getJson :: LBS.ByteString -> WaiSession st SResponse
getJson path = request methodGet (LBS.toStrict path) authHeaders ""

-- | Decode a servant @Either e a@ response body.
decodeEither :: (FromJSON e, FromJSON a) => SResponse -> Either String (Either e a)
decodeEither resp = maybe (Left (show (simpleBody resp))) Right (decode (simpleBody resp))

-- | Fail the test with a message when the value is missing.
require :: String -> Maybe a -> WaiSession st a
require msg = maybe (liftIO (ioError (userError msg))) pure

gamePath :: GameId -> LBS.ByteString
gamePath gid = "/1/onitama/games/" <> encode gid

apiSpec :: Spec
apiSpec = with mkApp $ describe "HTTP API: game against the AI" $ do
  it "creates a game, rejects an illegal pass and answers a legal move" $ do
    resp <- postJson "/1/onitama/games" (encode (NewGameRequest True BaseOnly))
    created <- require ("game creation failed: " ++ show (simpleBody resp)) $
      case decodeEither resp :: Either String (Either JoinError NewGameResponse) of
        Right (Right r) -> Just r
        _ -> Nothing
    let gid = newGameId created
        game0 = responseGame (newGameJoinResponse created)
        cards = gameCards game0

    gs <- require "cannot replay the new game" $ replayGame cards (gameHistory game0)
    liftIO $ gsNextColor gs `shouldBe` White

    -- A pass is illegal at the start of the game.
    let (c1, _) = gsWhiteCards gs
    passResp <- postJson (gamePath gid <> "/moves") (encode (formatMove (Move White Pass c1)))
    liftIO $ (decodeEither passResp :: Either String (Either MoveError MoveNotation)) `shouldBe` Right (Left MEInvalidMove)

    -- A legal move is accepted and the AI answers right away.
    let myMove = formatMove (head (legalMoves gs))
    moveResp <- postJson (gamePath gid <> "/moves") (encode myMove)
    liftIO $ (decodeEither moveResp :: Either String (Either MoveError MoveNotation)) `shouldBe` Right (Right myMove)

    after <- getJson (gamePath gid)
    g <- require ("cannot decode game: " ++ show (simpleBody after)) (decode (simpleBody after) :: Maybe GameWithNames)
    liftIO $ do
      length (gameHistory g) `shouldBe` length (gameHistory game0) + 2
      drop 1 (gameHistory g) `shouldBe` myMove : gameHistory game0
      fmap gsNextColor (replayGame cards (gameHistory g)) `shouldBe` Just White
  where
    mkApp = serve apiWithAssets <$> makeServer Nothing True noCleanup 1 1 False
    noCleanup = Database.CleanupConfig 0 0 0
