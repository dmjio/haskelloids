{-# LANGUAGE OverloadedStrings #-}
module Main where

import Affection as A
import SDL (($=))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.Map as M

import Linear as L

import NanoVG hiding (V2(..), V4(..))

import Control.Monad.IO.Class (liftIO)

-- internal imports

import Types
import Commons
import StateMachine ()
import Init

main :: IO ()
main = do
  logIO A.Debug "Starting"
  withAffection AffectionConfig
    { initComponents = All
    , windowTitle    = "Haskelloids"
    , windowConfig   = SDL.defaultWindow
      { SDL.windowOpenGL = Just SDL.defaultOpenGL
        { SDL.glProfile        = SDL.Core SDL.Normal 3 3
        , SDL.glColorPrecision = V4 8 8 8 8
        }
      }
    , initScreenMode = SDL.Windowed
    , canvasSize     = Nothing
    , loadState      = load
    , preLoop        = pre >> smLoad Menu
    , eventLoop      = handle
    , updateLoop     = update
    , drawLoop       = draw
    , cleanUp        = (\_ -> return ())
    }

pre :: Affection UserData ()
pre = do
  subs <- subsystems <$> getAffection
  liftIO $ logIO A.Debug "Setting global resize event listener"
  _ <- partSubscribe (subWindow subs) $ \msg -> case msg of
    MsgWindowResize _ _ (V2 w h) -> do
      let nw = floor $ fromIntegral h * (800/600)
          dw = floor $ (fromIntegral w - fromIntegral nw) / 2
      GL.viewport $= (GL.Position dw 0, GL.Size nw h)
    _ -> return ()
  return ()

update :: Double -> Affection UserData ()
update sec = do
  ud <- getAffection
  smUpdate (state ud) sec

handle :: [SDL.EventPayload] -> Affection UserData ()
handle e = do
  (Subsystems w k) <- subsystems <$> getAffection
  _ <- consumeSDLEvents w =<< consumeSDLEvents k e
  return ()

draw :: Affection UserData ()
draw = do
  ud <- getAffection
  liftIO $ beginFrame (nano ud) 800 600 (800/600)
  smDraw (state ud)
  liftIO $ endFrame (nano ud)
