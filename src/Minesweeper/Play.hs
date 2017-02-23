{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Minesweeper.Play
       (play)
       where

import           Minesweeper.Rendering
import           Minesweeper.Types
import           Minesweeper.Utils

import           ClassyPrelude         hiding (filter)

import           Control.Monad.State
import           Data.Witherable


play :: GameCfg -> GameState -> IO Condition
play cfg st = flip runReaderT cfg . flip evalStateT st $ playTurn

playTurn :: ( MonadIO m, MonadCatch m
            , MonadReader GameCfg m, MonadState GameState m )
         => m Condition
playTurn = draw >> queryAction >>= updateState >> checkContinue >>= loop
    where loop cont = case cont of
            Continue -> playTurn
            end      -> pure end

queryAction :: (MonadIO m, MonadCatch m, MonadReader GameCfg m)
            => m Action
queryAction = asks keymap >>= getInput

updateState :: (MonadState GameState m, MonadThrow m)
            => Action -> m ()
updateState (Move dir) = do
  st <- get
  let cur = cursor st
      st' = st { cursor = moveCursor dir cur }
  put st'
updateState Mark = do
  st <- get
  let (Cursor loc) = cursor st
      (Field ovl)  = overlay st
  when (lookup loc ovl == Just Warning) $ throw WarningPresent
  markPosition Warning True
updateState Dig = markPosition Free False

markPosition :: (MonadState GameState m)
             => Marking -> Bool -> m ()
markPosition m toggle = do
  st <- get
  let (Cursor loc) = cursor st
      (Field ovl) = overlay st
      st' = st { overlay = Field $ alterMap
                           (\val -> case val of
                                      Nothing   -> Just m
                                      Just mark -> if toggle
                                        then Nothing
                                        else Just mark)
                                    loc ovl }
  put st'

checkContinue :: (MonadReader GameCfg m, MonadState GameState m)
              => m Condition
checkContinue = do
  st <- get
  (Field mines) <- asks minefield
  let (Field ovl) = overlay st
      frees = filter (Free ==) ovl
      gameOver = if intersection (keysSet mines) (keysSet frees) /= mempty
        then Just GameOver
        else Nothing
      gameSuccess = let ideal = keysSet $ differenceMap (unfield $ fieldOf Free) mines
                        real  = keysSet $ frees
                    in if ideal == real
                          then Just GameSuccess
                          else Nothing
  pure $ maybe Continue id (gameOver <|> gameSuccess)

moveCursor :: Direction -> Cursor -> Cursor
moveCursor dir (Cursor loc) = Cursor $ case dir of
  North -> first pred loc
  South -> first succ loc
  West  -> second pred loc
  East  -> second succ loc
