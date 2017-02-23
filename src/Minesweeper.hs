module Minesweeper
       (game)
       where

import           Minesweeper.Play
import           Minesweeper.Setup
import           Minesweeper.Terminate

import           ClassyPrelude

import           System.IO

game :: IO ()
game = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  say "Welcome to MINESWEEPER"
  setup >>= uncurry play >>= end
