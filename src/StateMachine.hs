{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateMachine where

import Affection

import Types
import InGame
import Menu

instance StateMachine State UserData where
  smLoad Menu = loadMenu (smClean Menu >> smLoad InGame)

  smLoad InGame = loadGame (smClean InGame >> smLoad Menu) (smClean InGame >> smLoad InGame)

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
