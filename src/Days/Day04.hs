module Days.Day04 (runDay) where

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
import Control.Monad
import Data.Set (fromList)
import Data.Set (size)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' (pair <* endOfLine)
  where
    range = do
      l <- decimal
      char '-'
      r <- decimal
      return (l, r)
    pair = do
      l <- range
      char ','
      r <- range
      return (l, r)

------------ TYPES ------------
type Range = (Int, Int)

type Pair = (Range, Range)

type Input = [Pair]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
contains :: Range -> Range -> Bool
contains (l, r) (x, y) = l <= x && y <= r

partA :: Input -> OutputA
partA = length . filter (ap ((||) . uncurry contains) (uncurry . flip $ contains))

------------ PART B ------------

overlaps :: Range -> Range -> Bool
overlaps (l, r) (x, y) = l <= y && r >= x

partB :: Input -> OutputB
partB = length . filter (ap ((||) . uncurry overlaps) (uncurry . flip $ overlaps))
