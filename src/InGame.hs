{-# LANGUAGE RecordWildCards #-}
module InGame where

import Affection as A
import qualified SDL

import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, fromJust, isNothing)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import System.Random (randomRIO)

import Linear

import Types
import Commons
import Menu

loadGame :: Affection UserData () -> Affection UserData ()
loadGame stateChange = do
  liftIO $ logIO A.Debug "loading game"
  ud <- getAffection
  nhs <- newHaskelloids
  kid <- partSubscribe (subKeyboard $ subsystems ud)
    (\kbdev -> when (msgKbdKeyMotion kbdev == SDL.Pressed) $
      case SDL.keysymKeycode (msgKbdKeysym kbdev) of
        SDL.KeycodeSpace -> do
          liftIO $ logIO Debug "TODO: PEW!"
        SDL.KeycodeR -> do
          liftIO $ logIO Debug "Reloading"
          putAffection ud
            { stateUUIDs = UUIDClean [] []
            }
          loadGame stateChange
        SDL.KeycodeEscape -> do
          liftIO $ logIO Debug "Leave to Menu"
          stateChange
        _ -> return ()
    )
  putAffection ud
    { stateUUIDs = UUIDClean [] [kid]
    , haskelloids = nhs
    , ship = (ship ud)
      { sPos = V2 400 300
      , sVel = V2 0 0
      , sRot = 0
      }
    , state = InGame
    }


updateGame :: Double -> Affection UserData ()
updateGame sec = do
  ud <- getAffection
  let nhs = map (updateHaskelloid sec) (haskelloids ud)
  putAffection ud
    { haskelloids = nhs
    , ship = updateShip sec (ship ud)
    }

updateShip :: Double -> Ship -> Ship
updateShip ddt s@Ship{..} = s
  { sPos = wrapAround (sPos + fmap (dt *) sVel) 40
  }
  where
    dt = realToFrac ddt

drawGame :: Affection UserData ()
drawGame = do
  ud <- getAffection
  mapM_ drawHaskelloid (haskelloids ud)
  drawShip (ship ud)

drawShip :: Ship -> Affection UserData ()
drawShip Ship{..} = do
  ctx <- nano <$> getAffection
  liftIO $ drawImage ctx (sImg) (sPos - fmap (/2) dim) dim sRot 255
  where
    dim = V2 40 40
