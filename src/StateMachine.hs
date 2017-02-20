{-# LANGUAGE MultiParamTypeClasses #-}

module StateMachine where

import Affection
import qualified SDL
import GEGL

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (foldM, when)

import System.Random (randomRIO)

import Types
import Commons
import InGame

class StateMachine a us where
  smLoad   :: a -> Affection us ()
  smUpdate :: a -> Double -> Affection us ()
  smEvent  :: a -> Double -> SDL.Event -> Affection us ()
  smDraw   :: a -> Affection us ()
  smClean  :: a -> Affection us ()

instance StateMachine State UserData where
  smLoad Menu = do
    ud <- getAffection
    liftIO $ gegl_node_link
      (nodeGraph ud M.! KeyMenuOver)
      (nodeGraph ud M.! KeyFGNop)
    hs <- liftIO $ catMaybes <$> foldM (\acc _ -> do
      px <- randomRIO (0, 800)
      py <- randomRIO (0, 600)
      insertHaskelloid acc Nothing (px, py)
      ) [] ([0..9] :: [Int])
    liftIO $ gegl_node_link_many $ map hFlange hs
    liftIO $ gegl_node_link (last $ map hFlange hs) (nodeGraph ud M.! KeyHNop)
    putAffection ud
      { haskelloids = hs
      , fade = FadeIn 1
      , state = Menu
      }

  smLoad InGame = loadGame

  smUpdate Menu sec = do
    ud <- getAffection
    nhs <- mapM (updateHaskelloid sec) (haskelloids ud)
    case fade ud of
      FadeIn ttl -> do
        liftIO $ gegl_node_set (nodeGraph ud M.! KeyMenuText) $
          Operation "gegl:text"
            [ Property "color" $ PropertyColor $ GEGL.RGBA 1 1 1 (1.1 - ttl)
            ]
        putAffection ud
          { fade = if (ttl - sec) > 0 then FadeIn (ttl - sec) else FadeOut 1
          , haskelloids = nhs
          }
      FadeOut ttl -> do
        liftIO $ gegl_node_set (nodeGraph ud M.! KeyMenuText) $
          Operation "gegl:text"
            [ Property "color" $ PropertyColor $ GEGL.RGBA 1 1 1 ttl
            ]
        putAffection ud
          { fade = if (ttl - sec) > 0 then FadeOut (ttl - sec) else FadeIn 1
          , haskelloids = nhs
          }

  smUpdate InGame sec = updateGame sec

  smEvent Menu _ e = do
    ad <- get
    case SDL.eventPayload e of
      SDL.KeyboardEvent dat ->
        case SDL.keysymKeycode $ SDL.keyboardEventKeysym dat of
          SDL.KeycodeSpace ->
            when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $ do
              ud <- getAffection
              liftIO $ gegl_node_disconnect (nodeGraph ud M.! KeyFGNop) "input"
              smLoad InGame
          _ -> return ()
      SDL.WindowClosedEvent _ ->
        put ad
          { quitEvent = True
          }
      _ -> return ()

  smEvent InGame sec e = handleGameEvent sec e 

  smDraw Menu = do
    ud <- getAffection
    liftIO $ gegl_node_process $ nodeGraph ud M.! KeySink
    present
      (GeglRectangle 0 0 800 600)
      (buffer ud)
      True

  smDraw InGame = drawGame
