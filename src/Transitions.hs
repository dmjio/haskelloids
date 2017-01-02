{-# LANGUAGE MultiParamTypeClasses #-}

module Transitions where

import Affection
import GEGL

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (foldM)

import System.Random (randomRIO)

import Types
import Commons

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

  smUpdate Menu sec = do
    ud <- getAffection
    case fade ud of
      FadeIn ttl -> do
        liftIO $ gegl_node_set (nodeGraph ud M.! KeyMenuText) $
          Operation "gegl:text"
            [ Property "color" $ PropertyColor $ GEGL.RGBA 1 1 1 (1.1 - ttl)
            ]
        putAffection ud
          { fade = if (ttl - sec) > 0 then FadeIn (ttl - sec) else FadeOut 1
          }
      FadeOut ttl -> do
        liftIO $ gegl_node_set (nodeGraph ud M.! KeyMenuText) $
          Operation "gegl:text"
            [ Property "color" $ PropertyColor $ GEGL.RGBA 1 1 1 ttl
            ]
        putAffection ud
          { fade = if (ttl - sec) > 0 then FadeOut (ttl - sec) else FadeIn 1
          }
