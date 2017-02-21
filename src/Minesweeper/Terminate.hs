module Minesweeper.Terminate where

import           ClassyPrelude

import           Minesweeper.Types

import           System.Exit


end :: (MonadIO m)
    => Condition -> m ()
end GameOver    = do
  traverse_ say $
    [ " _______  _______  __   __  _______    _______  __   __  _______  ______"
    , "|       ||   _   ||  |_|  ||       |  |       ||  | |  ||       ||    _ |"
    , "|    ___||  |_|  ||       ||    ___|  |   _   ||  |_|  ||    ___||   | ||"
    , "|   | __ |       ||       ||   |___   |  | |  ||       ||   |___ |   |_||_"
    , "|   ||  ||       ||       ||    ___|  |  |_|  ||       ||    ___||    __  |"
    , "|   |_| ||   _   || ||_|| ||   |___   |       | |     | |   |___ |   |  | |"
    , "|_______||__| |__||_|   |_||_______|  |_______|  |___|  |_______||___|  |_|"
    ]
  liftIO exitFailure
end GameSuccess = do
  traverse_ say $
    [ " __   __  _______  __   __    _     _  ___  __    _ "
    , "|  | |  ||       ||  | |  |  | | _ | ||   ||  |  | |"
    , "|  |_|  ||   _   ||  | |  |  | || || ||   ||   |_| |"
    , "|       ||  | |  ||  |_|  |  |       ||   ||       |"
    , "|_     _||  |_|  ||       |  |       ||   ||  _    |"
    , "  |   |  |       ||       |  |   _   ||   || | |   |"
    , "  |___|  |_______||_______|  |__| |__||___||_|  |__|"
    ]
  liftIO exitSuccess
end Continue    = error "Game is over but shouldn't be!"
