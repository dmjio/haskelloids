{-# LANGUAGE RecordWildCards #-}
module InGame where

import Affection as A
import qualified SDL

import qualified Data.Set as S
import Data.Maybe (isNothing, catMaybes)

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)

import System.Random (randomRIO)

import Linear

import NanoVG hiding (V2(..))

import Foreign.C.Types (CFloat(..))

import Types
import Commons

dVel :: Float
dVel = 100

pewVel :: Float
pewVel = 200

pewTTL :: Double
pewTTL = 10

dRot :: Float
dRot = 150

loadGame :: Affection UserData () -> Affection UserData () -> Affection UserData ()
loadGame stateChange clean = do
  liftIO $ logIO A.Debug "loading game"
  ud <- getAffection
  nhs <- newHaskelloids
  kid <- partSubscribe (subKeyboard $ subsystems ud) (handleGameKeys stateChange clean)
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

handleGameKeys :: Affection UserData () -> Affection UserData () -> KeyboardMessage -> Affection UserData ()
handleGameKeys stateChange clean kbdev = when (msgKbdKeyMotion kbdev == SDL.Pressed) $
  case SDL.keysymKeycode (msgKbdKeysym kbdev) of
    SDL.KeycodeSpace -> unless (msgKbdKeyRepeat kbdev) $ do
      liftIO $ logIO A.Debug "PEW!"
      shoot
    SDL.KeycodeR -> unless (msgKbdKeyRepeat kbdev) $ do
      ud <- getAffection
      unless (isNothing $ wonlost ud) $ do
        liftIO $ logIO A.Debug "Reloading"
        clean
    SDL.KeycodeEscape -> do
      liftIO $ logIO A.Debug "Leave to Menu"
      stateChange
    SDL.KeycodeW -> accelShip dVel
    SDL.KeycodeS -> accelShip (-dVel)
    SDL.KeycodeA -> rotateShip dRot
    SDL.KeycodeD -> rotateShip (-dRot)
    _ -> return ()

shoot :: Affection UserData ()
shoot = do
  ud <- getAffection
  let Ship{..} = ship ud
      npew = Pew ppos pVel pewTTL
      ppos = sPos + (V2 0 25 `rotVec` sRot)
      pVel = sVel + (V2 0 pewVel `rotVec` sRot)
  putAffection ud
    { shots = npew : shots ud
    }

accelShip :: Float -> Affection UserData ()
accelShip vel = do
  ud <- getAffection
  dt <- getDelta
  let s    = ship ud
      nVel = sVel s + fmap (realToFrac dt *) (V2 0 vel `rotVec` sRot s)
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
      { sRot = sRot (ship ud) - deg * realToFrac dt
      }
    }

rotVec :: (Num a, Floating a) => V2 a -> a -> V2 a
rotVec (V2 x y) deg = V2 nx ny
  where
    nx   = x * cos (dtor deg) + y * sin (dtor deg)
    ny   = x * sin (dtor deg) - y * cos (dtor deg)
    dtor = (pi / 180 *)

updateGame :: Double -> Affection UserData ()
updateGame sec = do
  ud <- getAffection
  let nhs = map (updateHaskelloid sec) (haskelloids ud)
      npews = filter (\p -> pTTL p > 0) $ map (updatePew sec) (shots ud)
  putAffection ud
    { haskelloids = nhs
    , shots = npews
    , ship = updateShip sec (ship ud)
    }
  checkShotDown
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
  when (isNothing (wonlost ud2) && null (haskelloids ud2))
    (putAffection ud2
      { wonlost = Just Won
      }
    )

checkShotDown :: Affection UserData ()
checkShotDown =
  do
    ud <- getAffection
    let shoots = shots ud
        hasks = haskelloids ud
        pairs = catMaybes $ concatMap (crossOut hasks) shoots
        deadHasks = map fst pairs
        nhask = foldl (\acc a -> filter (\x -> a /= x) acc) hasks deadHasks
        npews = foldl (\acc a -> filter (\x -> a /= x) acc) shoots (map snd pairs)
    children <- liftIO $ concat <$> mapM (\Haskelloid{..} -> do
      n1velx <- randomRIO (-10, 10)
      n1vely <- randomRIO (-10, 10)
      n1rot <- randomRIO (-180, 180)
      n1pitch <- randomRIO (-pi, pi)
      n2velx <- randomRIO (-10, 10)
      n2vely <- randomRIO (-10, 10)
      n2rot <- randomRIO (-180, 180)
      n2pitch <- randomRIO (-pi, pi)
      let ndiv = hDiv + 1
      if ndiv > 5
      then return []
      else return
        [ Haskelloid hPos (V2 n1velx n1vely) n1rot n1pitch ndiv hImg
        , Haskelloid hPos (V2 n2velx n2vely) n2rot n2pitch ndiv hImg
        ]
      ) deadHasks
    putAffection ud
      { shots = npews
      , haskelloids = nhask ++ children
      }
  where
    crossOut :: [Haskelloid] -> Pew -> [Maybe (Haskelloid, Pew)]
    crossOut hs p =
      foldl (\acc h ->
        if distance (pPos p) (hPos h) < (50 / fromIntegral (hDiv h))
        then Just (h, p) : acc
        else Nothing : acc
        ) [] hs

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

updatePew :: Double -> Pew -> Pew
updatePew ddt p@Pew{..} = p
  { pPos = wrapAround (pPos + fmap (dt *) pVel) 0
  , pTTL = pTTL - ddt
  }
  where
    dt = realToFrac ddt

drawGame :: Affection UserData ()
drawGame = do
  ud <- getAffection
  mapM_ drawHaskelloid (haskelloids ud)
  mapM_ drawPew (shots ud)
  case wonlost ud of
    Just x -> drawWonLost x
    Nothing -> drawShip (ship ud)

drawWonLost :: WonLost -> Affection UserData ()
drawWonLost wl = do
  ctx <- nano <$> getAffection
  liftIO $ do
    let color = case wl of
          Won -> rgba 128 255 0 255
          Lost -> rgba 255 128 0 255
        textStr = case wl of
          Won -> "YOU WON!"
          Lost -> "YOU LOsT!"
    save ctx
    fontSize ctx 120
    fontFace ctx "modulo"
    textAlign ctx (S.fromList [AlignCenter,AlignTop])
    fillColor ctx (rgba 255 255 255 255)
    textBox ctx 0 200 800 textStr
    fillColor ctx color
    fontSize ctx 40
    textBox ctx 0 350 800 "Press [Esc] to exit\nPress [R] to try again"
    restore ctx

drawShip :: Ship -> Affection UserData ()
drawShip Ship{..} = do
  ctx <- nano <$> getAffection
  liftIO $ drawImage ctx sImg (sPos - fmap (/2) dim) dim sRot 255
  where
    dim = V2 40 40

drawPew :: Pew -> Affection UserData ()
drawPew Pew{..} = do
  ctx <- nano <$> getAffection
  liftIO $ do
    let (V2 x y) = fmap CFloat pPos
    save ctx
    beginPath ctx
    fillColor ctx (rgba 255 128 0 255)
    circle ctx x y 2
    fill ctx
    restore ctx
