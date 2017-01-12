{-# LANGUAGE OverloadedStrings #-}
module Main where

import Affection
import qualified SDL
import GEGL

import Data.List (delete)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (when, foldM)

import Debug.Trace

-- internal imports

import Types
import Commons
import StateMachine

import Menu
import InGame

main :: IO ()
main = withAffection AffectionConfig
  { initComponents = All
  , windowTitle    = "Haskelloids"
  , windowConfig   = defaultWindow
  , preLoop        = pre
  , drawLoop       = draw
  , updateLoop     = update
  , loadState      = load
  , cleanUp        = clean
  }

pre :: Affection UserData ()
pre = do
  smLoad Menu
  -- ud <- getAffection
  -- liftIO $ gegl_node_process (nodeGraph ud M.! KeySink)
  -- present
  --   (GeglRectangle 0 0 800 600)
  --   (buffer ud)
  --   True

update :: Double -> Affection UserData ()
update sec = do
  wd <- getAffection
  smUpdate (state wd) sec
  evs <- SDL.pollEvents
  mapM_ (smEvent (state wd) sec) evs

draw :: Affection UserData ()
draw = do
  ud <- getAffection
  smDraw $ state ud
