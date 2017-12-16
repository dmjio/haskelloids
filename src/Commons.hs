{-# LANGUAGE RecordWildCards #-}

module Commons where

import Affection
import qualified SDL
import GEGL
import BABL

import qualified Data.Map as M
import Data.List (delete)
import Data.Maybe (catMaybes, isJust)

import Control.Monad (foldM, unless, when)

import System.Random (randomRIO)

import Debug.Trace

import Types

toR :: Double -> Double
toR deg = deg * pi / 180

wrapAround :: (Ord t, Num t) => (t, t) -> t -> (t, t)
wrapAround (nx, ny) width = (nnx, nny)
  where
    nnx
      | nx > 800    = nx - (800 + width)
      | nx < -width = nx + 800 + width
      | otherwise   = nx
    nny
      | ny > 600    = ny - (600 + width)
      | ny < -width = ny + 600 + width
      | otherwise   = ny
