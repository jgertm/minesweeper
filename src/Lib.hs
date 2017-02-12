{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -W #-}

module Lib where

import           ClassyPrelude

import           Data.Array.IArray


type Location = (Int,Int)

data Field e = Field (Array Location e) deriving (Show)

type Minefield = Field Mine
type Mine = Maybe ()

type Overlay = Field (Maybe Marking)
data Marking = Warning
             | Free
             deriving (Show, Eq)
