module Minesweeper
       (game)
       where

import           Minesweeper.Play
import           Minesweeper.Setup
import           Minesweeper.Terminate

import           ClassyPrelude

game :: IO ()
game = do
  say "Welcome to MINESWEEPER"
  setup >>= uncurry play >>= end
