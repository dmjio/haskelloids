module InGame where

import Affection
import qualified SDL
import GEGL

import qualified Data.Map as M

import Control.Monad (when)

import Debug.Trace

import Types
import Commons

handleGameEvent :: Double -> SDL.Event -> Affection UserData ()
handleGameEvent sec e = do
  ad <- get
  wd <- getAffection
  case SDL.eventPayload e of
    SDL.KeyboardEvent dat ->
      case (SDL.keysymKeycode $ SDL.keyboardEventKeysym dat) of
        SDL.KeycodeLeft -> do
          ud <- getAffection
          when (SDL.keyboardEventKeyMotion dat == SDL.Pressed && not (wonlost ud)) $
            putAffection ud
              { ship = (ship ud)
                { sRot = (sRot $ ship ud) + 270 * sec
                }
              }
        SDL.KeycodeRight -> do
          ud <- getAffection
          when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
            putAffection ud
              { ship = (ship ud)
                { sRot = (sRot $ ship ud) - 270 * sec
                }
              }
        SDL.KeycodeUp ->
          when (SDL.keyboardEventKeyMotion dat == SDL.Pressed && not (wonlost wd)) $ do
            ud <- getAffection
            let vx = -10 * (sin (toR $ (sRot $ ship ud))) + fst (sVel $ ship ud)
                vy = -10 * (cos (toR $ (sRot $ ship ud))) + snd (sVel $ ship ud)
            putAffection ud
              { ship = (ship ud)
                { sVel = (vx, vy)
                }
              }
            -- traceM $ show (vx, vy) ++ " " ++ show (sRot $ ship ud)
        SDL.KeycodeSpace ->
          when (SDL.keyboardEventKeyMotion dat == SDL.Pressed && not (wonlost wd)) $ do
            ud <- getAffection
            liftIO $ gegl_node_set (nodeGraph ud M.! KeyPixelize) $ Operation "gegl:pixelize"
              [ Property "size-x" $ PropertyInt 8
              , Property "size-y" $ PropertyInt 8
              ]
            -- ad <- get
            let posX = (fst $ sPos $ ship ud) + 23 - 35 * sin (toR $ sRot $ ship ud)
                posY = (snd $ sPos $ ship ud) + 23 - 35 * cos (toR $ sRot $ ship ud)
            tempRoot <- liftIO $ gegl_node_new
            tempRect <- liftIO $ gegl_node_new_child tempRoot $ Operation "gegl:rectangle"
              [ Property "x" $ PropertyDouble $ posX
              , Property "y" $ PropertyDouble $ posY
              , Property "width" $ PropertyDouble 4
              , Property "height" $ PropertyDouble 4
              , Property "color" $ PropertyColor $ (GEGL.RGBA 1 1 1 1)
              ]
            tempOver <- liftIO $ gegl_node_new_child tempRoot $ defaultOverOperation
            _ <- liftIO $ gegl_node_connect_to tempRect "output" tempOver "aux"
            ips <- insertParticle (shots ud) $
              Particle
                { particleTimeToLive = 5
                , particleCreation = elapsedTime ad
                , particlePosition = (posX, posY)
                , particleRotation = Rad 0
                , particleVelocity =
                  -- ( (floor $ -200 * (sin $ toR $ (sRot $ ship ud) + (fst $ sVel $ ship ud)))
                  -- , (floor $ -200 * (cos $ toR $ (sRot $ ship ud) + (snd $ sVel $ ship ud)))
                  ( (floor $ -200 * (sin $ toR $ sRot $ ship ud))
                  , (floor $ -200 * (cos $ toR $ sRot $ ship ud))
                  )
                , particlePitchRate = Rad 0
                , particleRootNode = tempRoot
                , particleNodeGraph = M.fromList
                  [ ("root", tempRoot)
                  , ("over", tempOver)
                  , ("rect", tempRect)
                  ]
                , particleStackCont = tempOver
                , particleDrawFlange = tempOver
                }
            putAffection $ ud
              { shots = ips
              , pixelSize = 8
              }
        SDL.KeycodeR ->
          when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $ do
            liftIO $ traceIO "reloading"
            liftIO $ clean wd
            nd <- liftIO $ load $ drawSurface ad
            putAffection nd
        _ -> return ()
    SDL.WindowClosedEvent _ -> do
      traceM "seeya!"
      put ad
        { quitEvent = True
        }
    _ -> return ()
