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
import InGame
import Menu

instance StateMachine State UserData where
  smLoad Menu = loadMenu (smClean Menu >> smLoad InGame)

  smLoad InGame = loadGame (smClean InGame >> smLoad Menu)

  smUpdate Menu = updateMenu

  smUpdate InGame = updateGame

  smDraw Menu = drawMenu

  smDraw InGame = drawGame

  smEvent _ _ = return ()

  smClean _ = do
    ud <- getAffection
    let (UUIDClean uuwin uukbd) = stateUUIDs ud
        (Subsystems win kbd)    = subsystems ud
    mapM_ (partUnSubscribe win) uuwin
    mapM_ (partUnSubscribe kbd) uukbd
    putAffection ud
      { stateUUIDs = UUIDClean [] []
      }
