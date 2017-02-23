{-# LANGUAGE OverloadedStrings #-}

module Minesweeper.Keymap where

import           Minesweeper.Types

import           ClassyPrelude


explain :: (MonadIO m, Show a) => Keymap a -> m ()
explain (Keymap km) = do
  say "Please enter one of the following:"
  traverse_ (\(c,a) -> say $ mconcat ["(", singleton c, ")\t", tshow a]) $ mapToList km
  putStr "> "

pick :: Keymap (Keymap Action)
pick = Keymap $ mapFromList
  [ ('v', vim)
  , ('n', naive)
  ]

difficulty :: Keymap Hardness
difficulty = Keymap $ mapFromList
  [ ('b', Balkans)
  , ('i', Iraq)
  , ('k', Korea)
  ]

naive, vim :: Keymap Action
naive = Keymap $ mapFromList
  [ ('n', Move North)
  , ('s', Move South)
  , ('e', Move East)
  , ('w', Move West)
  , ('m', Mark)
  , ('d', Dig)
  ]
vim = Keymap $ mapFromList
  [ ('k', Move North)
  , ('j', Move South)
  , ('h', Move West)
  , ('l', Move East)
  , ('v', Mark)
  , ('i', Dig)
  ]
