module Minesweeper.Setup
       (setup)
       where

import           ClassyPrelude

import qualified Minesweeper.Keymap   as Keymap
import           Minesweeper.Types
import           Minesweeper.Utils

import           Control.Monad.Random


setup :: (MonadIO m, MonadRandom m, MonadCatch m)
      => m (GameCfg, GameState)
setup = do
  scheme   <- queryKeymap
  hardness <- queryHardness
  field    <- newMineField hardness
  markings <- newOverlay
  let cfg = Cfg field scheme
      st  = State markings $ Cursor (0,0)
  pure (cfg, st)

queryKeymap :: (MonadIO m, MonadCatch m)
            => m (Keymap Action)
queryKeymap = do
  say "Choose a keymap."
  getInput Keymap.pick

queryHardness :: (MonadIO m, MonadCatch m)
              => m Hardness
queryHardness = do
  say "Choose difficulty."
  getInput Keymap.difficulty

newMineField :: (MonadRandom m)
             => Hardness -> m Minefield
newMineField hardness = do
  locations <- generateLocations (hardnessLevel hardness) mempty
  let mineField  = foo locations (\c -> (c, Mine))
  pure . Field $ mineField
    where foo :: [a] -> (a -> (Location, Mine)) -> Map Location Mine
          foo ls f = mapFromList $ fmap f ls

generateLocations :: (MonadRandom m)
                  => Int -> Set Location -> m [Location]
generateLocations n ls = if length ls == n
  then pure $ setToList ls
  else do
    e <- getRandomR (lowest,highest)
    generateLocations n $ insertSet e ls

hardnessLevel :: Hardness -> Int
hardnessLevel level = case level of
  Balkans -> 10
  Iraq    -> 20
  Korea   -> 30

newOverlay :: (MonadIO m)
           => m Overlay
newOverlay = pure $ Field mempty
