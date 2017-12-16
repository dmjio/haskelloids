module Menu where

import Affection as A
import qualified SDL

import Debug.Trace

import Data.Maybe

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M

import NanoVG

-- internal imports

import Types
import Commons

handleMenuEvent :: (Affection UserData ()) -> [SDL.EventPayload] -> Affection UserData ()
handleMenuEvent loader es =
  mapM_ (\e ->
    case e of
      SDL.KeyboardEvent dat ->
        case SDL.keysymKeycode $ SDL.keyboardEventKeysym dat of
          SDL.KeycodeSpace ->
            when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $ do
              ud <- getAffection
              loader
          _ -> return ()
      SDL.WindowClosedEvent _ -> do
        ad <- get
        put ad
          { quitEvent = True
          }
      _ -> return ()
    ) es

loadMenu :: Affection UserData ()
loadMenu = do
  liftIO $ logIO A.Debug "Loading Menu"
  ud <- getAffection
  mhaskImage <- liftIO $
    createImage (nano ud) (FileName "assets/haskelloid.svg") 0
  when (isNothing mhaskImage) $
    liftIO $ logIO Error "Failed to load asset haskelloid"
  hs <- newHaskelloids (fromJust mhaskImage)
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
