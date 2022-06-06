module App.ScoreTable (ScoreTable, fromScoreTable, lookupTable, createTable) where

import Data.List (group, sort, sortOn)
import Data.Maybe (fromMaybe)

-- Represent probability tables (analogous to frequency tables)
newtype ScoreTable a = Table {fromScoreTable :: [(a, Double)]}
  deriving (Show, Eq)

-- Funcs --

indices :: ScoreTable a -> [a]
indices (Table x) = map fst x

vals :: ScoreTable a -> [Double]
vals (Table x) = map snd x

lookupTable :: Eq a => a -> ScoreTable a -> Maybe Double
lookupTable entry (Table tbl) = lookup entry tbl

createTable :: Ord a => [a] -> ScoreTable a
createTable ls = Table $ map f cntTbl
  where
    f (x, y) = (x, prob y * (1 - prob y))
    prob x = fromIntegral x / fromIntegral lsLength
    lsLength = length ls
    cntTbl = map (\x -> (head x, length x)) . group . sort $ ls

sortTable :: Ord a => ScoreTable a -> ScoreTable a
sortTable (Table tbl) = Table $ sortOn fst tbl

fTable :: (Double -> Double) -> ScoreTable a -> ScoreTable a
fTable f (Table tbl) = Table $ map g tbl
  where
    g (x, y) = (x, f y)

leftJoinTable ::
  Ord a =>
  (Double -> Double -> Double) ->
  ScoreTable a ->
  ScoreTable a ->
  ScoreTable a
leftJoinTable f l r = Table $ zip iLeft newVals
  where
    newVals = zipWith f (vals l) matchedValsR
    matchedValsR = map (\x -> fromMaybe 0 (lookupTable x r)) iLeft
    iLeft = indices l

rightjoinTable ::
  Ord a =>
  (Double -> Double -> Double) ->
  ScoreTable a ->
  ScoreTable a ->
  ScoreTable a
rightjoinTable f = flip (leftJoinTable f)