module Menu where

import Affection as A
import qualified SDL

import Debug.Trace

import Data.Maybe

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M

import NanoVG hiding (V2(..))

import Linear

import Foreign.C.Types

-- internal imports

import Types
import Commons

handleMenuEvent :: (Affection UserData ()) -> [SDL.EventPayload] -> Affection UserData ()
handleMenuEvent _ es = do
  (Subsystems w k) <- subsystems <$> getAffection
  _ <- consumeSDLEvents w =<< consumeSDLEvents k es
  return ()

loadMenu :: Affection UserData ()
loadMenu = do
  liftIO $ logIO A.Debug "Loading Menu"
  ud <- getAffection
  mhaskImage <- liftIO $
    createImage (nano ud) (FileName "assets/haskelloid.svg") 0
  when (isNothing mhaskImage) $
    liftIO $ logIO Error "Failed to load asset haskelloid"
  hs <- newHaskelloids (fromJust mhaskImage)
  _ <- partSubscribe (subKeyboard $ subsystems ud)
    (\kbdev -> case SDL.keysymKeycode (msgKbdKeysym kbdev) of
      SDL.KeycodeEscape -> do
        liftIO $ logIO A.Debug "seeya"
        quit
      SDL.KeycodeF -> do
        when (msgKbdKeyMotion kbdev == SDL.Pressed) $ do
          liftIO $ logIO A.Debug "screen toggling"
          toggleScreen
      _ -> return ()
      )
  putAffection ud
    { haskelloids = hs
    , fade = FadeIn 1
    , state = Menu
    -- , shots = (shots ud)
    --   { partSysParts = ParticleStorage Nothing [] }
    }

updateMenu :: Double -> Affection UserData ()
updateMenu sec = do
  ud <- getAffection
  let nhs = map (updateHaskelloid sec) (haskelloids ud)
  case fade ud of
    FadeIn ttl -> do
      putAffection ud
        { fade = if (ttl - sec) > 0 then FadeIn (ttl - sec) else FadeOut 1
        , haskelloids = nhs
        }
    FadeOut ttl -> do
      putAffection ud
        { fade = if (ttl - sec) > 0 then FadeOut (ttl - sec) else FadeIn 1
        , haskelloids = nhs
        }

drawMenu :: Affection UserData ()
drawMenu = do
  ud <- getAffection
  let V2 sx sy = fmap (CFloat . realToFrac) (sPos $ ship ud)
  liftIO $ do
    save (nano ud)
    sPaint <- imagePattern (nano ud) 400 300 20 20 0 (sImg $ ship ud) 255
    beginPath (nano ud)
    rect (nano ud) 400 300 20 20
    fillPaint (nano ud) sPaint
    fill (nano ud)
    restore (nano ud)
  dt <- getElapsedTime
  liftIO $
    drawSpinner (nano ud) 100 100 100 (CFloat $ realToFrac dt)

drawSpinner :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawSpinner vg cx cy r t = do
  let a0 = 0+t*6
      a1 = pi + t*6
      r0 = r
      r1 = r*0.75
  save vg

  beginPath vg
  arc vg cx cy r0 a0 a1 CW
  arc vg cx cy r1 a1 a0 CCW
  closePath vg
  let ax = cx+cos a0 * (r0+r1)*0.5
      ay = cy+sin a0 * (r0+r1)*0.5
      bx = cx+cos a1 * (r0+r1)*0.5
      by = cy+sin a1 * (r0+r1)*0.5
  paint <- linearGradient vg ax ay bx by (rgba 255 255 255 0) (rgba 255 255 255 128)
  fillPaint vg paint
  fill vg

  restore vg
