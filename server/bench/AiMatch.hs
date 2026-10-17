-- | Let two minimax players of different depth play against each other.
--
-- Run with @make ai-match@ or, with options:
--
-- > stack exec -- ai-match --white-depth 3 --black-depth 5 --games 20 --seed 1 --cards mixed
--
-- Each game is deterministic for a given seed, so results are reproducible.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, when)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Minimax
import Onitama
import Options.Applicative
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Random (mkStdGen)
import Text.Printf (printf)
import Types (CardSet (..), Color (..))

data CardChoice = Base | Expansion | Mixed deriving (Show)

data Opts = Opts
  { optWhiteDepth :: Int,
    optBlackDepth :: Int,
    optGames :: Int,
    optSeed :: Int,
    optCards :: CardChoice,
    optMaxPlies :: Int,
    optVerbose :: Bool
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> option auto (long "white-depth" <> short 'w' <> value 3 <> showDefault <> help "Search depth of White")
    <*> option auto (long "black-depth" <> short 'b' <> value 5 <> showDefault <> help "Search depth of Black (the server AI uses 5)")
    <*> option auto (long "games" <> short 'n' <> value 20 <> showDefault <> help "Number of games")
    <*> option auto (long "seed" <> short 's' <> value 1 <> showDefault <> help "Seed of the first game; game i uses seed + i")
    <*> option cardChoice (long "cards" <> short 'c' <> value Mixed <> showDefaultWith (const "mixed") <> help "base | expansion | mixed (alternating)")
    <*> option auto (long "max-plies" <> value 200 <> showDefault <> help "Count a game as a draw after this many plies")
    <*> switch (long "verbose" <> short 'v' <> help "Print every move")
  where
    cardChoice = eitherReader $ \s -> case s of
      "base" -> Right Base
      "expansion" -> Right Expansion
      "mixed" -> Right Mixed
      _ -> Left "expected base, expansion or mixed"

data GameResult = GameResult
  { grWinner :: Maybe Color,
    grPlies :: Int,
    grPasses :: Int,
    grTimes :: [(Color, Double)] -- search time in ms per move
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser (info (optsParser <**> helper) (fullDesc <> progDesc "Pit two minimax depths against each other"))
  printf "White depth %d vs Black depth %d, %d games\n\n" (optWhiteDepth opts) (optBlackDepth opts) (optGames opts)

  results <- forM [0 .. optGames opts - 1] $ \i -> do
    let seed = optSeed opts + i
        cardSet = case optCards opts of
          Base -> BaseOnly
          Expansion -> WithExpansion
          Mixed -> if even i then BaseOnly else WithExpansion
        cards = dealCards cardSet (mkStdGen seed)
    r <- playGame opts (initGameState cards)
    printf
      "game %3d  seed %4d  %-13s  %-11s after %3d plies%s  cards: %s\n"
      (i + 1)
      seed
      (show cardSet)
      (maybe "draw" ((++ " wins") . show) (grWinner r))
      (grPlies r)
      (if grPasses r > 0 then printf "  (%d passes)" (grPasses r) else "" :: String)
      (unwords cards)
    pure r

  let count c = length (filter ((== c) . grWinner) results)
      timesOf c = [t | r <- results, (c', t) <- grTimes r, c' == c]
      avg xs = if null xs then 0 else sum xs / fromIntegral (length xs)
      mx xs = if null xs then 0 else maximum xs
  putStrLn ""
  printf "%-6s %6s %6s %12s %12s\n" "" "depth" "wins" "avg ms/move" "max ms/move"
  printf "%-6s %6d %6d %12.1f %12.1f\n" "White" (optWhiteDepth opts) (count (Just White)) (avg (timesOf White)) (mx (timesOf White))
  printf "%-6s %6d %6d %12.1f %12.1f\n" "Black" (optBlackDepth opts) (count (Just Black)) (avg (timesOf Black)) (mx (timesOf Black))
  printf "%-6s %6s %6d\n" "Draws" "" (count Nothing)
  printf "\nPasses played: %d\n" (sum (map grPasses results))

playGame :: Opts -> GameState -> IO GameResult
playGame opts = go 0 0 []
  where
    go plies passes times gs
      | Just w <- gsWinner gs = pure (GameResult (Just w) plies passes (reverse times))
      | plies >= optMaxPlies opts = pure (GameResult Nothing plies passes (reverse times))
      | otherwise = do
          let color = gsNextColor gs
              depth = if color == White then optWhiteDepth opts else optBlackDepth opts
          start <- getCurrentTime
          mv <- evaluate (force (Minimax.bestMove depth gs))
          end <- getCurrentTime
          let ms = realToFrac (diffUTCTime end start) * 1000 :: Double
          case mv of
            Nothing -> do
              printf "  !! %s found no move at ply %d: %s\n" (show color) plies (show gs)
              pure (GameResult Nothing plies passes (reverse times))
            Just m -> do
              when (optVerbose opts) $ printf "  %3d. %-24s %7.1f ms\n" (plies + 1) (formatMove m) ms
              let passes' = if moveAction m == Pass then passes + 1 else passes
              case applyMove m gs of
                Nothing -> do
                  printf "  !! illegal move chosen: %s\n" (formatMove m)
                  pure (GameResult Nothing plies passes' (reverse times))
                Just gs' -> go (plies + 1) passes' ((color, ms) : times) gs'
