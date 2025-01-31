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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  let range = (,) <$> decimal <* char '-' <*> decimal
      pair = (,) <$> range <* char ',' <*> range
   in many' (pair <* endOfLine)

------------ TYPES ------------
type Range = (Int, Int)

type Pair = (Range, Range)

type Input = [Pair]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
contains :: Range -> Range -> Bool
contains (l, r) (x, y) = l <= x && y <= r

-- | util function to check f for both orders of ranges
both f = ap ((||) . uncurry f) (uncurry . flip $ f)

partA :: Input -> OutputA
partA = length . filter (both contains)

------------ PART B ------------

overlaps :: Range -> Range -> Bool
overlaps (l, r) (x, y) = l <= y && r >= x

partB :: Input -> OutputB
partB = length . filter (both overlaps)
