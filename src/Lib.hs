{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -W #-}

module Lib where

import           ClassyPrelude

import           Control.Monad.Random


type Location = (Int,Int)

lowest, highest :: Location
lowest  = (0,0)
highest = (9,9)

instance (Random l, Bounded l, Random r, Bounded r) => Random (l,r) where
  randomR ((lowL,lowR),(hiL, hiR)) gen = do
    let (randL, gen')  = randomR (lowL, hiL) gen
        (randR, gen'') = randomR (lowR, hiR) gen'
    ((randL,randR), gen'')
  random gen = randomR ((minBound,minBound), (maxBound,maxBound)) gen

data Field e = Field { unfield ::  (Map Location e) } deriving (Show)
instance (Semigroup e) => Semigroup (Field e) where
  (Field x) <> (Field y) = Field $ union x y
instance (Monoid e) => Monoid (Field e) where
  mempty = Field . mapFromList . fmap (\i -> (quotRem i 10, mempty)) $ [0..99]
  mappend (Field x) (Field y) = Field $ union x y
instance Functor Field where
  fmap f (Field e) = Field $ fmap f e

fieldOf :: e -> Field e
fieldOf e = Field . mapFromList . fmap (\i -> (quotRem i 10, e)) $ [0..99]

type Minefield = Field Mine
type Mine = ()

data Hardness = Balkans
              | Iraq
              | Korea

hardnessLevel :: Hardness -> Int
hardnessLevel Balkans = 10
hardnessLevel Iraq    = 20
hardnessLevel Korea   = 30

newMineField :: (MonadRandom m) => Hardness -> m Minefield
newMineField hardness = do
  mineLocations <- generateLocations (hardnessLevel hardness) mempty
  let mineField  = foo mineLocations (\c -> (c, ()))
  pure . Field $ mineField
  where foo :: [a] -> (a -> (Location, Mine)) -> Map Location Mine
        foo ls f = mapFromList $ fmap f ls

generateLocations :: (MonadRandom m) => Int -> Set Location -> m [Location]
generateLocations n ls = if length ls == n
  then pure $ setToList ls
  else do
    e <- getRandomR (lowest,highest)
    generateLocations n $ insertSet e ls
type Overlay = Field Marking
data Marking = Warning
             | Free
             deriving (Show, Eq)
