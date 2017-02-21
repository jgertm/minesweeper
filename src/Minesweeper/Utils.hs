{-# LANGUAGE OverloadedStrings #-}

module Minesweeper.Utils where

import           Minesweeper.Keymap
import           Minesweeper.Types

import           ClassyPrelude

import           System.IO          (getChar)


retry :: (MonadCatch m, Exception e)
      => m a -> (e -> m ()) -> m a
retry action report = catch action $ \e -> report e >> retry action report

interpret :: (MonadThrow m)
          => Keymap a -> Char -> m a
interpret (Keymap km) input = case lookup input km of
  Just res -> pure res
  Nothing  -> throw $ UnknownInput input

getKey :: (MonadIO m)
       => m Char
getKey = liftIO getChar

getInput :: (MonadIO m, MonadCatch m
            , Show a)
         => Keymap a -> m a
getInput km = do
  explain km
  putStr "> "
  input <- getKey
  retry (interpret km input) $ \(UnknownInput i) -> say $ "Unrecognized input: " <> singleton i