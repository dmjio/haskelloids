{-# LANGUAGE ForeignFunctionInterface #-}
module Init where

import Affection as A

import SDL (($=))
import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.Set as S
import Data.Maybe

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM

import System.Exit (exitFailure)

import Linear

import NanoVG hiding (V2)

import Foreign.C.Types (CInt(..))

-- Internal imports

import Types

foreign import ccall unsafe "glewInit"
  glewInit :: IO CInt

load :: IO UserData
load = do
  -- liftIO $ logIO A.Debug "Let's drop some Hints for SDL"
  -- SDL.HintRenderDriver $= SDL.OpenGL
  SDL.cursorVisible $= False
  GL.clearColor $= GL.Color4 0 0 0.1 1
  liftIO $ logIO A.Debug "init GLEW"
  _ <- glewInit
  liftIO $ logIO A.Debug "loading state"
  liftIO $ logIO A.Debug "create context"
  nvgCtx <- createGL3 (S.fromList [Antialias, StencilStrokes, NanoVG.Debug])
  liftIO $ logIO A.Debug "load ship image"
  mshipImage <- createImage nvgCtx (FileName "assets/ship.png") 0
  when (isNothing mshipImage) $ do
    logIO Error "Failed to load asset ship"
    exitFailure
  mhaskImage <- liftIO $
    createImage nvgCtx (FileName "assets/haskelloid.png") 0
  when (isNothing mhaskImage) $
    liftIO $ logIO Error "Failed to load asset haskelloid"
  mfont <- createFont nvgCtx "modulo" (FileName "assets/Modulo.ttf")
  when (isNothing mfont) $ do
    logIO Error "Failed to load font"
    exitFailure
  liftIO $ logIO A.Debug "Initializing subsystems"
  subs <- Subsystems
    <$> (Window <$> newTVarIO [])
    <*> (Keyboard <$> newTVarIO [])
  liftIO $ logIO A.Debug "Setting viewport"
  GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
  liftIO $ logIO A.Debug "Returning UserData"
  return UserData
    { ship = Ship
      { sPos = V2 400 300
      , sVel = V2 0 0
      , sRot = 0
      , sImg = fromJust mshipImage
      }
    , haskelloids = []
    , shots = []
    , wonlost = Nothing
    , state = Menu
    , fade = FadeIn 1
    , nano = nvgCtx
    , font = fromJust mfont
    , subsystems = subs
    , haskImage = fromJust mhaskImage
    , stateUUIDs = UUIDClean [] []
    }
