module Days.Day03 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Data.Char
import Debug.Trace
import Data.List.Split (chunksOf)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' (many' letter <* endOfLine)

------------ TYPES ------------
type Input = [[Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error "invalid char"

halve :: [a] -> ([a], [a])
halve xs = splitAt s xs
  where
    s = length xs `div` 2

partA :: Input -> OutputA
partA = sum . map (sum . map priority . nub . uncurry intersect . halve)

------------ PART B ------------

partB :: Input -> OutputB
partB = sum . map (sum . map priority . nub . foldr1 intersect) . chunksOf 3
