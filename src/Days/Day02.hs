module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (choice)
import Data.Void
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = line `sepBy` endOfLine
  where
    line = do
      opp <- letter
      char ' '
      my <- letter
      return (opp, my)

------------ TYPES ------------
type Input = [(Char, Char)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
score :: Char -> Int
score 'A' = 1
score 'B' = 2
score 'C' = 3

win :: Char -> Char
win 'A' = 'B'
win 'B' = 'C'
win 'C' = 'A'

outcome :: Char -> Char -> Int
outcome opp my
  | opp == my = 3
  | opp == win my = 0
  | otherwise = 6

outcomeScore opp my = score my + outcome opp my

translate 'X' = 'A'
translate 'Y' = 'B'
translate 'Z' = 'C'

partA :: Input -> OutputA
partA = sum . map (\(opp, my) -> outcomeScore opp (translate my))

------------ PART B ------------

lose :: Char -> Char
lose 'A' = 'C'
lose 'B' = 'A'
lose 'C' = 'B'

choice :: Char -> Char -> Char
choice 'X' = lose
choice 'Y' = id
choice 'Z' = win

outcomeScore' (opp, my) = let my' = choice my opp in score my' + outcome opp my'

partB :: Input -> OutputB
partB = sum . map outcomeScore'
