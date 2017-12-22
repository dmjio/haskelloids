module Menu where

import Affection as A
import qualified SDL

import qualified Data.Set as S

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import NanoVG

-- internal imports

import Types
import Commons

loadMenu :: Affection UserData () -> Affection UserData ()
loadMenu stateChange = do
  liftIO $ logIO A.Debug "Loading Menu"
  ud <- getAffection
  hs <- newHaskelloids
  kbdUUID <- partSubscribe (subKeyboard $ subsystems ud)
    (\kbdev -> when (msgKbdKeyMotion kbdev == SDL.Pressed) $
      case SDL.keysymKeycode (msgKbdKeysym kbdev) of
        SDL.KeycodeEscape -> do
          liftIO $ logIO A.Debug "seeya"
          quit
        SDL.KeycodeSpace -> do
          liftIO $ logIO A.Debug "Leaving Menu to Game"
          stateChange
        _ -> return ()
      )
  putAffection ud
    { haskelloids = hs
    , fade = FadeIn 1
    , state = Menu
    , stateUUIDs = UUIDClean [] [kbdUUID]
    -- , shots = (shots ud)
    --   { partSysParts = ParticleStorage Nothing [] }
    }

updateMenu :: Double -> Affection UserData ()
updateMenu sec = do
  ud <- getAffection
  let nhs = map (updateHaskelloid sec) (haskelloids ud)
  case fade ud of
    FadeIn ttl ->
      putAffection ud
        { fade = if (ttl - sec) > 0 then FadeIn (ttl - sec) else FadeOut 1
        , haskelloids = nhs
        }
    FadeOut ttl ->
      putAffection ud
        { fade = if (ttl - sec) > 0 then FadeOut (ttl - sec) else FadeIn 1
        , haskelloids = nhs
        }

drawMenu :: Affection UserData ()
drawMenu = do
  ud <- getAffection
  mapM_ drawHaskelloid (haskelloids ud)
  liftIO $ do
    let ctx = nano ud
        alpha fio = case fio of
          FadeIn d  -> floor (255 * (1 - d))
          FadeOut d -> floor (255 * d)
    save ctx
    fontSize ctx 120
    fontFace ctx "modulo"
    textAlign ctx (S.fromList [AlignCenter,AlignTop])
    fillColor ctx (rgba 255 255 255 255)
    textBox ctx 0 200 800 "HASKELLOIDS"
    fillColor ctx (rgba 0 128 255 (alpha $ fade ud))
    fontSize ctx 40
    textBox ctx 0 350 800 "Press [Space] to Play\nPress [Esc] to exit"
    restore ctx
