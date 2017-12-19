module Menu where

import Affection as A
import qualified SDL

import Debug.Trace

import Data.Maybe
import qualified Data.Set as S

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M

import NanoVG hiding (V2(..), V4(..))

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
    createImage (nano ud) (FileName "assets/haskelloid.png") 0
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
  mapM_ drawHaskelloid (haskelloids ud)
  liftIO $ do
    let ctx = nano ud
        alpha fio = case fio of
          FadeIn d  -> (floor (255 * (1 - d)))
          FadeOut d -> (floor (255 * d))
    save ctx
    fontSize ctx 120
    fontFace ctx "modulo"
    textAlign ctx (S.fromList [AlignCenter,AlignTop])
    -- (Bounds (V4 b0 b1 b2 b3)) <- textBoxBounds ctx x y' 150 "HASKELLOIDS"
    fillColor ctx (rgba 255 255 255 255)
    textBox ctx 0 200 800 "HASKELLOIDS"
    fillColor ctx (rgba 255 128 0 (alpha $ fade ud))
    fontSize ctx 40
    textBox ctx 0 350 800 "Press [Space] to PLay\nPress [Esc] to exit"
    restore ctx
  -- t <- getElapsedTime
  -- liftIO $ drawSpinner (nano ud) 100 100 100 t
