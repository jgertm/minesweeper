{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Minesweeper.Rendering
       (draw)
       where

import           Minesweeper.Types

import           ClassyPrelude

import           Control.Monad.State
import           Data.Monoid         (Sum (..))


class Renderable r where
  render   :: r -> String

instance (Renderable r) => Renderable (Field r) where
  render f = unlines . fmap (concatMap snd)
           . groupBy ((==) `on` (fst . fst))
           . mapToList . unfield
           $ fmap render f <> fieldOf "_"

instance Renderable Int where
  render i = take 1 $ show i

instance Renderable Mine where
  render Mine = "+"

instance Renderable Marking where
  render Warning = "!"
  render Free    = " "

instance Renderable Cursor where
  render _ = "#"

draw :: (MonadReader GameCfg m, MonadState GameState m, MonadIO m)
     => m ()
draw = do
  cfg <- ask
  st  <- get

  say mempty
  sayString $ unlines . fmap mconcat . rows . fmap (\loc -> (loc, pixel cfg st loc)) $ allLocations

pixel :: GameCfg -> GameState -> Location -> String
pixel cfg st loc = do
  let neighborCount = proximal cfg loc
      neighborsHere = if neighborCount > 0
                        then Just neighborCount
                        else Nothing

  let markingHere = lookup loc . unfield . overlay $ st

      c@(Cursor curLoc) = cursor st
      cursorHere        = if curLoc == loc
                            then Just c
                            else Nothing

  drawTopmost cursorHere markingHere neighborsHere

drawTopmost :: Maybe Cursor -> Maybe Marking -> Maybe Int -> String
drawTopmost cursor marking neighbors = maybe "_" (take 1) $
  render <$> cursor <|>
  render <$> marking <|>
  render <$> neighbors

proximal :: GameCfg -> Location -> Int
proximal cfg loc = do
  let Field mf = minefield cfg
      ones     = fmap (const $ Sum 1) mf
  maybe 0 getSum . concatMap (`lookup` ones) . neighbors $ loc

neighbors :: (Bifunctor p, Enum a, Enum b)
          => p a b -> [p a b]
neighbors loc = fmap ($ loc) $ (.) <$> fmap first [pred, id, succ] <*> fmap second [pred, id, succ]


rows :: (Eq a)
     => [((a,b),e)] -> [[e]]
rows = fmap (fmap snd) . groupBy ((==) `on` ffst)
  where ffst = fst . fst
