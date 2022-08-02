module App.ScoreTable
  ( ScoreTable
  , fromScoreTable
  , lookupTable
  , createTable
  , zipTable
  , leftJoin
  , rightJoin
  , sortTable
  , maxTable
  , indices
  , vals
  , fTable
  ) where

import           Data.List  (group, sort, sortOn, union)
import           Data.Maybe (fromMaybe)

-- Represent probability tables (analogous to frequency tables)
newtype ScoreTable a = Table {fromScoreTable :: [(a, Double)]}
  deriving (Show, Eq)

-- Instances --
instance Ord a => Semigroup (ScoreTable a) where
  x <> y = fullJoin (+) x y

instance Ord a => Monoid (ScoreTable a) where
  mempty = Table []

instance Functor ScoreTable where
  fmap f (Table tbl) = Table $ map g tbl
    where
      g (x, y) = (f x, y)

-- Funcs --

indices :: ScoreTable a -> [a]
indices (Table x) = map fst x

vals :: ScoreTable a -> [Double]
vals (Table x) = map snd x

lookupTable :: Eq a => a -> ScoreTable a -> Maybe Double
lookupTable entry (Table tbl) = lookup entry tbl

zipTable :: Ord a => [a] -> [Double] -> ScoreTable a
zipTable x y = Table $ zip x y

createTable :: Ord a => [a] -> ScoreTable a
createTable ls = Table $ map f cntTbl
 where
  f (x, y) = (x, prob y * (1 - prob y))
  prob x = fromIntegral x / fromIntegral lsLength
  lsLength = length ls
  cntTbl   = map (\x -> (head x, length x)) . group . sort $ ls

sortTable :: Ord a => ScoreTable a -> ScoreTable a
sortTable (Table tbl) = Table $ sortOn snd tbl

fTable :: (Double -> Double) -> ScoreTable a -> ScoreTable a
fTable f (Table tbl) = Table $ map g tbl
  where
    g (x, y) = (x, f y)

fullJoin
  :: Ord a
  => (Double -> Double -> Double)
  -> ScoreTable a
  -> ScoreTable a
  -> ScoreTable a
fullJoin f l r = zipTable indices' newVals
  where
    newVals = zipWith f v1 v2
    v1 = map (g l) indices'
    v2 = map (g r) indices'
    g x = fromMaybe 0 . (`lookupTable` x)
    indices' = indices l `union` indices r

leftJoin
  :: Ord a
  => (Double -> Double -> Double)
  -> ScoreTable a
  -> ScoreTable a
  -> ScoreTable a
leftJoin f l r = Table $ zip iLeft newVals
 where
  newVals      = zipWith f (vals l) matchedValsR
  matchedValsR = map (\x -> fromMaybe 0 (lookupTable x r)) iLeft
  iLeft        = indices l

rightJoin
  :: Ord a
  => (Double -> Double -> Double)
  -> ScoreTable a
  -> ScoreTable a
  -> ScoreTable a
rightJoin f = flip $ leftJoin f

maxTable :: Ord a => ScoreTable a -> (a, Double)
maxTable = last . fromScoreTable . sortTable
