{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minesweeper.Types where

import           ClassyPrelude

import           Control.Monad.Random
import           Control.Monad.State


-- # game monad
type GameM a = StateT GameState (ReaderT GameCfg IO) a

-- ## monadic arguments
data GameCfg = Cfg { minefield :: Minefield
                   , keymap    :: Keymap Action}
             deriving (Show)

data GameState = State { overlay :: Overlay
                       , cursor  :: Cursor }
               deriving (Show)

-- # domain errors
data Error = UnknownInput Char
           | WarningPresent
           deriving (Show)
instance Exception Error


-- # game states
data Condition = Continue
               | GameOver
               | GameSuccess


-- # game difficulty
data Hardness = Balkans
              | Iraq
              | Korea
              deriving (Show)


-- # game field
data Field e = Field { unfield ::  (Map Location e) }
             deriving (Show)

instance (Semigroup e) => Semigroup (Field e) where
  (Field x) <> (Field y) = Field $ x <> y

instance (Monoid e) => Monoid (Field e) where
  mempty = Field . mapFromList . fmap (\i -> (quotRem i 10, mempty)) $ [0..99]
  mappend (Field x) (Field y) = Field $ union x y

instance Functor Field where
  fmap f (Field e) = Field $ fmap f e

fieldOf :: e -> Field e
fieldOf e = Field . mapFromList . fmap (\i -> (quotRem i 10, e)) $ [0..99]

-- ## mine field
type Minefield = Field Mine
data Mine = Mine
          deriving (Show)


-- # annotation field
type Overlay = Field Marking
data Marking = Warning
             | Free
             deriving (Show, Eq)


type Location = (Int,Int)

lowest, highest :: Location
lowest  = (0,0)
highest = (9,9)

allLocations :: [Location]
allLocations = fmap (`quotRem` 10) [0..99]


-- # user actions
data Action = Move Direction
            | Mark
            | Dig
            deriving (Show, Eq)

-- ## movement directions
data Direction = North
               | South
               | East
               | West
            deriving (Show, Eq, Ord)

-- ## input schemes
newtype Keymap a = Keymap (Map Char a)
                 deriving (Show)


-- # orphan instances
instance (Random l, Bounded l, Random r, Bounded r) => Random (l,r) where
  randomR ((lowL,lowR),(hiL, hiR)) gen = do
    let (randL, gen')  = randomR (lowL, hiL) gen
        (randR, gen'') = randomR (lowR, hiR) gen'
    ((randL,randR), gen'')
  random gen = randomR ((minBound,minBound), (maxBound,maxBound)) gen
newtype Cursor = Cursor { uncursor :: Location }
               deriving (Show)
