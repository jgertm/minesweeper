{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -W #-}

module Lib where

import           ClassyPrelude

import           Control.Monad.Random
import           Data.Array.IArray


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

data Field e = Field (Array Location e) deriving (Show)

type Minefield = Field Mine
type Mine = Maybe ()

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
  let mineField  = foo mineLocations (\c -> (c, Just ()))
      clearField = foo [0..99] (\i -> (quotRem i 10,Nothing))
  pure . Field . array (lowest,highest) . mapToList $ union mineField clearField
  where foo :: [a] -> (a -> (Location, Mine)) -> Map Location Mine
        foo ls f = mapFromList $ fmap f ls

generateLocations :: (MonadRandom m) => Int -> Set Location -> m [Location]
generateLocations n ls = if length ls == n
  then pure $ setToList ls
  else do
    e <- getRandomR (lowest,highest)
    generateLocations n $ insertSet e ls
type Overlay = Field (Maybe Marking)
data Marking = Warning
             | Free
             deriving (Show, Eq)
