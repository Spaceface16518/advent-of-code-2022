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
import Data.Attoparsec.Text
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
      opp <- oppMove <$> letter
      char ' '
      my <- myMove <$> letter
      return (opp, my)

oppMove :: Char -> Move
oppMove 'A' = Rock
oppMove 'B' = Paper
oppMove 'C' = Scissors

myMove :: Char -> Move
myMove 'X' = Rock
myMove 'Y' = Paper
myMove 'Z' = Scissors

------------ TYPES ------------
type Input = [(Move, Move)]

type OutputA = Int

type OutputB = Int

data Move = Rock | Paper | Scissors deriving (Show, Eq)

------------ PART A ------------
moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

outCome :: Move -> Move -> Int
outCome Rock Scissors = 0
outCome Paper Rock = 0
outCome Scissors Paper = 0
outCome a b = if a == b then 3 else 6

totalScore :: (Move, Move) -> Int
totalScore (opp, my) = moveScore my + outCome opp my

partA :: Input -> OutputA
partA = sum . map totalScore

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
