module InGame where

import Affection
import qualified SDL
import GEGL

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (when, foldM)

import System.Random (randomRIO)

import Debug.Trace

import Types
import Commons

loadGame :: Affection UserData ()
loadGame = do
  ud <- getAffection
  _ <- liftIO $ gegl_node_connect_to
    (nodeGraph ud M.! KeyShipTranslate)
    "output"
    (nodeGraph ud M.! KeyShipOver)
    "aux"
  hs <- liftIO $ catMaybes <$> foldM (\acc _ -> do
    coords <- liftIO excludeShip
    insertHaskelloid acc Nothing coords
    ) [] ([0..9] :: [Int])
  liftIO $ gegl_node_link_many $ map hFlange hs
  liftIO $ gegl_node_link (last $ map hFlange hs) (nodeGraph ud M.! KeyHNop)
  putAffection ud
    { haskelloids = hs
    , wonlost = False
    , shots = ParticleSystem
      (ParticleStorage Nothing [])
      (nodeGraph ud M.! KeyPNop)
      (buffer ud)
    , ship = Ship
      { sPos = (375, 275)
      , sVel = (0, 0)
      , sRot = 0
      , sFlange = (nodeGraph ud M.! KeyShipRotate)
      }
    , pixelSize = 3
    , state = InGame
    }

excludeShip :: IO (Double, Double)
excludeShip = do
  px <- randomRIO (0, 800)
  py <- randomRIO (0, 600)
  inter <- gegl_rectangle_intersect
    (GeglRectangle px py 100 100)
    (GeglRectangle 350 250 100 100) -- Ship's starting position and size
  case inter of
    Just _ ->
      excludeShip
    Nothing ->
      return (fromIntegral px, fromIntegral py)

updateGame :: Double -> Affection UserData ()
updateGame sec = do
  ad <- get
  ud <- getAffection
  when (((floor $ elapsedTime ad :: Int) * 100) `mod` 10 < 2 && pixelSize ud > 3) $ do
    pd <- getAffection
    liftIO $ gegl_node_set (nodeGraph pd M.! KeyPixelize) $ Operation "gegl:pixelize"
      [ Property "size-x" $ PropertyInt $ pixelSize pd - 1
      , Property "size-y" $ PropertyInt $ pixelSize pd - 1
      ]
    putAffection ud
      { pixelSize = pixelSize ud -1
      }
  let nx = (fst $ sPos $ ship ud) + (fst $ sVel $ ship ud) * sec
      ny = (snd $ sPos $ ship ud) + (snd $ sVel $ ship ud) * sec
      (nnx, nny) = wrapAround (nx, ny) 50
  liftIO $ gegl_node_set (nodeGraph ud M.! KeyShipTranslate) $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble $ nnx
    , Property "y" $ PropertyDouble $ nny
    ]
  liftIO $ gegl_node_set (nodeGraph ud M.! KeyShipRotate) $ Operation "gegl:rotate"
    [ Property "degrees" $ PropertyDouble $ sRot $ ship ud
    ]
  td <- getAffection
  putAffection td
    { ship = (ship ud)
      { sPos = (nnx, nny)
      }
    }
  ud2 <- getAffection
  nhs <- mapM (updateHaskelloid sec) (haskelloids ud2)
  putAffection ud2
    { haskelloids = nhs
    }
  liftIO $ traceIO $ show $ length nhs
  ud3 <- getAffection
  ups <- updateParticleSystem (shots ud3) sec shotsUpd shotsDraw
  ud4 <- getAffection
  putAffection ud4
    { shots = ups
    }

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
