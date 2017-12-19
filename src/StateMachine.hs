{-# LANGUAGE MultiParamTypeClasses #-}

module StateMachine where

import Affection
import qualified SDL

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (foldM, when)

import System.Random (randomRIO)

import Types
import Commons
-- import InGame
import Menu

instance StateMachine State UserData where
  smLoad Menu = loadMenu

  -- smLoad InGame = loadGame

  smUpdate Menu = updateMenu

  -- smUpdate InGame sec = updateGame sec

  smDraw Menu = drawMenu

  -- smDraw InGame = drawGame

  smEvent _ _ = return ()
