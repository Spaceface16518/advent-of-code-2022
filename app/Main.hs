module Main where

--- Day imports

--- Other imports

import Control.Applicative.Combinators qualified as C (option)
import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Days.Day01 qualified as Day01 (runDay)
import Days.Day02 qualified as Day02 (runDay)
import Days.Day03 qualified as Day03 (runDay)
import Days.Day04 qualified as Day04 (runDay)
import Days.Day05 qualified as Day05 (runDay)
import Days.Day06 qualified as Day06 (runDay)
import Days.Day07 qualified as Day07 (runDay)
import Days.Day08 qualified as Day08 (runDay)
import Days.Day09 qualified as Day09 (runDay)
import Days.Day10 qualified as Day10 (runDay)
import Days.Day11 qualified as Day11 (runDay)
import Days.Day12 qualified as Day12 (runDay)
import Days.Day13 qualified as Day13 (runDay)
import Days.Day14 qualified as Day14 (runDay)
import Days.Day15 qualified as Day15 (runDay)
import Days.Day16 qualified as Day16 (runDay)
import Days.Day17 qualified as Day17 (runDay)
import Days.Day18 qualified as Day18 (runDay)
import Days.Day19 qualified as Day19 (runDay)
import Days.Day20 qualified as Day20 (runDay)
import Days.Day21 qualified as Day21 (runDay)
import Days.Day22 qualified as Day22 (runDay)
import Days.Day23 qualified as Day23 (runDay)
import Days.Day24 qualified as Day24 (runDay)
import Days.Day25 qualified as Day25 (runDay)
import Options.Applicative
-- Data Output

import Program.Color (withColor)
import Program.RunDay (Day, Verbosity (Quiet, Timings, Verbose))
import System.Console.ANSI (Color (..))
import Text.Printf (printf)

data Days
  = AllDays
  | OneDay
      { day :: Int,
        input :: Maybe String
      }
  deriving (Show)

data Options = Options Days Verbosity

dayParser :: Parser Days
dayParser = (OneDay <$> day <*> input) <|> allDays
  where
    day =
      option auto $
        long "day"
          <> short 'd'
          <> metavar "DAY"
          <> help "Present the solutions for one day."

    input =
      optional $
        strOption $
          long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "The file to read the selected day's input from."

    allDays =
      flag' AllDays $
        long "all-days"
          <> help
            ( unwords
                [ "Present solutions for all of the days of",
                  "Advent of Code, with default input file names."
                ]
            )

optionsParser :: Parser Options
optionsParser = Options <$> dayParser <*> verbosityParser
  where
    verbosityParser :: Parser Verbosity
    verbosityParser =
      C.option Quiet $
        ( flag' Verbose $
            long "verbose"
              <> short 'v'
              <> help
                ( unwords
                    [ "Whether to print out extra info, such as the",
                      "result of the input parser, and more detailed",
                      "error messages.",
                      "Also enables timing of solutions."
                    ]
                )
        )
          <|> ( flag' Timings $
                  long "timings"
                    <> short 't'
                    <> help
                      ( unwords
                          ["Whether to enable timing of the solutions."]
                      )
              )

days :: Map Int (Day, String)
days =
  Map.fromList . zip [1 ..] $
    [ (Day01.runDay, "input/Day01.txt"),
      (Day02.runDay, "input/Day02.txt"),
      (Day03.runDay, "input/Day03.txt"),
      (Day04.runDay, "input/Day04.txt"),
      (Day05.runDay, "input/Day05.txt"),
      (Day06.runDay, "input/Day06.txt"),
      (Day07.runDay, "input/Day07.txt"),
      (Day08.runDay, "input/Day08.txt"),
      (Day09.runDay, "input/Day09.txt"),
      (Day10.runDay, "input/Day10.txt"),
      (Day11.runDay, "input/Day11.txt"),
      (Day12.runDay, "input/Day12.txt"),
      (Day13.runDay, "input/Day13.txt"),
      (Day14.runDay, "input/Day14.txt"),
      (Day15.runDay, "input/Day15.txt"),
      (Day16.runDay, "input/Day16.txt"),
      (Day17.runDay, "input/Day17.txt"),
      (Day18.runDay, "input/Day18.txt"),
      (Day19.runDay, "input/Day19.txt"),
      (Day20.runDay, "input/Day20.txt"),
      (Day21.runDay, "input/Day21.txt"),
      (Day22.runDay, "input/Day22.txt"),
      (Day23.runDay, "input/Day23.txt"),
      (Day24.runDay, "input/Day24.txt"),
      (Day25.runDay, "input/Day25.txt")
    ]

performDay :: Options -> IO ()
performDay (Options d v) = case d of
  AllDays -> do
    results <-
      let eachDay d (dayFunc, inputFile) = do
            withColor Magenta $ putStrLn $ printf "\n***Day %02d***" d
            dayFunc v inputFile
       in sequence $ Map.mapWithKey eachDay days

    printSummary results
  OneDay {..} -> case days Map.!? day of
    Nothing -> putStrLn "Invalid day provided. There are 25 days in Advent."
    Just (dayFunc, inputFile) -> do
      let i' = fromMaybe inputFile input
      withColor Magenta $ putStrLn $ printf "\n***Day %02d***" day
      dayFunc v i'
      withColor Magenta $ putStrLn "************"

printSummary :: Map Int (Maybe Double, Maybe Double) -> IO ()
printSummary results = do
  putStrLn "\n************\n  Summary:  "
  let partsA = Map.mapKeys ((++ " (a)") . printf "%02d") $ fmap fst results
      partsB = Map.mapKeys ((++ " (b)") . printf "%02d") $ fmap snd results
      parts = Map.toList $ partsA <> partsB

      fails = [p | (p, Nothing) <- parts]
      fasts = [(p, t) | (p, Just t) <- parts, t < 1]
      slows = [(p, t) | (p, Just t) <- parts, t >= 1]

  putStr $ printf "\n%d parts " $ length fasts
  withColor Green $ putStr "completed in under 1 second"
  putStrLn ".\nOf the remainder:"
  unless (null fails) $ do
    putStr $ printf "  %d parts" $ length fails
    withColor Red $ putStr " failed"
    putStrLn $ ":\n    " ++ intercalate ", " fails
  unless (null slows) $ do
    putStr $ printf "  %d parts" $ length slows
    withColor Yellow $ putStr " took over 1 second to complete"
    putStrLn ":"
    forM_ slows $
      \(p, t) -> putStrLn $ printf "    %s took %.2f seconds" p t

main :: IO ()
main = performDay =<< execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Prints out some Advent of Code solutions.")
