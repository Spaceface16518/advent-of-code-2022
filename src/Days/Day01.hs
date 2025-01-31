module Days.Day01 (runDay) where

import Data.Attoparsec.Text hiding (take)
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Void
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` "\n" `sepBy` "\n\n"

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = foldr (max . sum) 0

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . take 3 . sortBy (flip compare) . map sum
