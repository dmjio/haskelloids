{-# LANGUAGE RecordWildCards #-}
module InGame where

import Affection as A
import qualified SDL

import qualified Data.Set as S
import Data.Maybe (isNothing)

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)

import Linear

import NanoVG hiding (V2(..))

import Types
import Commons

dVel :: Float
dVel = 100

dRot :: Float
dRot = 150

loadGame :: Affection UserData () -> Affection UserData ()
loadGame stateChange = do
  liftIO $ logIO A.Debug "loading game"
  ud <- getAffection
  nhs <- newHaskelloids
  kid <- partSubscribe (subKeyboard $ subsystems ud) (handleGameKeys stateChange)
  putAffection ud
    { stateUUIDs = UUIDClean [] [kid]
    , haskelloids = nhs
    , ship = (ship ud)
      { sPos = V2 400 300
      , sVel = V2 0 0
      , sRot = 0
      }
    , shots = []
    , state = InGame
    , wonlost = Nothing
    }

handleGameKeys :: Affection UserData () -> KeyboardMessage -> Affection UserData ()
handleGameKeys stateChange kbdev = when (msgKbdKeyMotion kbdev == SDL.Pressed) $
  case SDL.keysymKeycode (msgKbdKeysym kbdev) of
    SDL.KeycodeSpace -> unless (msgKbdKeyRepeat kbdev) $ do
      liftIO $ logIO A.Debug "TODO: PEW!"
    SDL.KeycodeR -> unless (msgKbdKeyRepeat kbdev) $ do
      ud <- getAffection
      liftIO $ logIO A.Debug "Reloading"
      putAffection ud
        { stateUUIDs = UUIDClean [] []
        }
      loadGame stateChange
    SDL.KeycodeEscape -> do
      liftIO $ logIO A.Debug "Leave to Menu"
      stateChange
    SDL.KeycodeW -> accelShip dVel
    SDL.KeycodeS -> accelShip (-dVel)
    SDL.KeycodeA -> rotateShip dRot
    SDL.KeycodeD -> rotateShip (-dRot)
    _ -> return ()

accelShip :: Float -> Affection UserData ()
accelShip vel = do
  ud <- getAffection
  dt <- getDelta
  let s    = ship ud
      nVel = sVel s + fmap (realToFrac dt *) ((V2 0 vel) `rotVec` sRot s)
  putAffection ud
    { ship = s
      { sVel = nVel
      }
    }

rotateShip :: Float -> Affection UserData ()
rotateShip deg = do
  ud <- getAffection
  dt <- getDelta
  putAffection ud
    { ship = (ship ud)
      { sRot = (sRot $ ship ud) - deg * realToFrac dt
      }
    }

rotVec :: (Num a, Floating a) => V2 a -> a -> V2 a
rotVec (V2 x y) deg = V2 nx ny
  where
    nx   = x * (cos $ dtor deg) + y * (sin $ dtor deg)
    ny   = x * (sin $ dtor deg) - y * (cos $ dtor deg)
    dtor = (pi / 180 *)

updateGame :: Double -> Affection UserData ()
updateGame sec = do
  ud <- getAffection
  let nhs = map (updateHaskelloid sec) (haskelloids ud)
  putAffection ud
    { haskelloids = nhs
    , ship = updateShip sec (ship ud)
    }
  ud2 <- getAffection
  when
    (  (  any (checkCollision (ship ud2)) (haskelloids ud2)
       || any (checkFriendlyFire (ship ud2)) (shots ud2)
       )
    && isNothing (wonlost ud2)
    )
    (putAffection ud2
      { wonlost = Just Lost
      }
    )

checkCollision :: Ship -> Haskelloid -> Bool
checkCollision s h =
  distance (sPos s) (hPos h) < minDist
  where
    minDist = 20 + (50 / fromIntegral (hDiv h))

checkFriendlyFire :: Ship -> Pew -> Bool
checkFriendlyFire s p =
  distance (sPos s) (pPos p) < 20

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
  case wonlost ud of
    Just Lost -> liftIO $ do
      let ctx = nano ud
      save ctx
      fontSize ctx 120
      fontFace ctx "modulo"
      textAlign ctx (S.fromList [AlignCenter,AlignTop])
      fillColor ctx (rgba 255 255 255 255)
      textBox ctx 0 200 800 "YOU LOST!"
      fillColor ctx (rgba 255 128 0 255)
      fontSize ctx 40
      textBox ctx 0 350 800 "Press [Esc] to exit\nPress [R] to try again"
      restore ctx
    Just Won -> liftIO $ do
      let ctx = nano ud
      save ctx
      fontSize ctx 120
      fontFace ctx "modulo"
      textAlign ctx (S.fromList [AlignCenter,AlignTop])
      fillColor ctx (rgba 255 255 255 255)
      textBox ctx 0 200 800 "YOU WON!"
      fillColor ctx (rgba 255 128 0 255)
      fontSize ctx 40
      textBox ctx 0 350 800 "Press [Esc] to exit"
      restore ctx
    Nothing -> drawShip (ship ud)

drawShip :: Ship -> Affection UserData ()
drawShip Ship{..} = do
  ctx <- nano <$> getAffection
  liftIO $ drawImage ctx (sImg) (sPos - fmap (/2) dim) dim sRot 255
  where
    dim = V2 40 40
