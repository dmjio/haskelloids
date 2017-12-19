{-# LANGUAGE OverloadedStrings #-}
module Main where

import Affection as A
import SDL (($=))
import qualified SDL

import qualified Data.Map as M

import Linear as L

import NanoVG

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
        { SDL.glProfile = SDL.Core SDL.Normal 3 3
        }
      }
    , initScreenMode = SDL.Windowed
    , canvasSize     = Nothing
    , loadState      = load
    , preLoop        = smLoad Menu
    , eventLoop      = handle
    , updateLoop     = update
    , drawLoop       = draw
    , cleanUp        = (\_ -> return ())
    }

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
  -- GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
  -- ud <- getAffection
  -- GL.currentProgram $= (Just . GLU.program $ program sd)
  -- let proj = ortho (-1) 1 (-1) 1 (-1) 1
  --     view = lookAt
  --       (V3 0 0 (-1))
  --       (V3 0 0 0)
  --       (V3 0 1 0)
  --     model = mkTransformation (Quaternion 1 (V3 0 0 0)) (V3 0 0 0)
