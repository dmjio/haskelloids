{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Affection
import qualified SDL
import GEGL

import Data.List (delete)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (when, foldM)

import Debug.Trace

-- internal imports

import Types
import InGame
import Commons

main :: IO ()
main = withAffection $ AffectionConfig
  { initComponents = All
  , windowTitle    = "Haskelloids"
  , windowConfig   = defaultWindow
  , initScreenMode = SDL.Windowed
  , preLoop        = return ()
  , drawLoop       = draw
  , updateLoop     = update
  , loadState      = load
  , cleanUp        = clean
  , canvasSize     = Nothing
  , eventLoop      = handleGameEvent
  }

update :: Double -> Affection UserData ()
update sec = do
  -- traceM $ (show $ 1 / sec) ++ " FPS"
  ad <- get
  wd <- getAffection
  when (((floor $ elapsedTime ad :: Int) * 100) `mod` 10 < 2 && pixelSize wd > 3) $ do
    liftIO $ gegl_node_set (nodeGraph wd M.! KeyPixelize) $ Operation "gegl:pixelize"
      [ Property "size-x" $ PropertyInt $ pixelSize wd - 1
      , Property "size-y" $ PropertyInt $ pixelSize wd - 1
      ]
    pd <- getAffection
    putAffection pd
      { pixelSize = pixelSize wd -1
      }
  -- evs <- SDL.pollEvents
  -- mapM_ (\e ->
  --   case state wd of
  --     InGame ->
  --       handleGameEvent sec e
  --     _ -> error "not yet implemented"
  --   ) evs
  ud2 <- getAffection
  nhs <- mapM (updateHaskelloid sec) (haskelloids ud2)
  -- liftIO $ traceIO $ show $ length nhs
  putAffection ud2
    { haskelloids = nhs
    }
  ud3 <- getAffection
  let nx = fst (sPos $ ship ud3) + (fst (sVel $ ship ud3)) * sec
      ny = snd (sPos $ ship ud3) + (snd (sVel $ ship ud3)) * sec
      (nnx, nny) = wrapAround (nx, ny) 50
  liftIO $ gegl_node_set (nodeGraph ud3 M.! KeyTranslate) $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble $ nnx
    , Property "y" $ PropertyDouble $ nny
    ]
  liftIO $ gegl_node_set (nodeGraph ud3 M.! KeyRotate) $ Operation "gegl:rotate"
    [ Property "degrees" $ PropertyDouble $ sRot $ ship ud3
    ]
  ups <- updateParticleSystem (shots ud3) sec shotsUpd
  ud4 <- getAffection
  putAffection ud4
    { ship = (ship ud3)
      { sPos = (nnx, nny)
      }
    , shots = ups
    }

wrapAround :: (Ord t, Num t) => (t, t) -> t -> (t, t)
wrapAround (nx, ny) width = (nnx, nny)
  where
    nnx =
      if nx > 800
      then nx - (800 + width)
      else if nx < -width then nx + 800 + width else nx
    nny =
      if ny > 600
      then ny - (600 + width)
      else if ny < -width then ny + 600 + width else ny

draw :: Affection UserData ()
draw = do
  ud <- getAffection
  drawParticleSystem (shots ud) (\_ _ _ -> return())
  liftIO $ gegl_node_process $ nodeGraph ud M.! KeySink
  present
    (GeglRectangle 0 0 800 600)
    (buffer ud)
    True
  render Nothing Nothing

shotsUpd :: Double -> Particle -> Affection UserData Particle
shotsUpd sec part@Particle{..} = do
  let newX = (fst particlePosition) + sec * (fromIntegral $ fst particleVelocity)
      newY = (snd particlePosition) + sec * (fromIntegral $ snd particleVelocity)
      (nnx, nny) = wrapAround (newX, newY) 4
  liftIO $ gegl_node_set (particleNodeGraph M.! "rect") $ Operation "gegl:rectangle"
    [ Property "x" $ PropertyDouble $ nnx
    , Property "y" $ PropertyDouble $ nny
    ]
  ud <- getAffection
  inters <- catMaybes <$> mapM (\h -> do
    col <- liftIO $ gegl_rectangle_intersect
      (GeglRectangle (floor nnx) (floor nny) 4 4)
      (GeglRectangle
        (floor $ fst $ hPos h)
        (floor $ snd $ hPos h)
        (100 `div` hDiv h)
        (100 `div` hDiv h)
        )
    case col of
      Just _ -> return $ Just h
      Nothing -> return Nothing
    ) (haskelloids ud)
  when (not $ null inters) $
    haskelloidShotDown $ head inters
  lost <- liftIO $ gegl_rectangle_intersect
    (GeglRectangle (floor nnx) (floor nny) 4 4)
    (GeglRectangle
      (floor $ fst $ sPos $ ship ud)
      (floor $ snd $ sPos $ ship ud)
      50
      50
      )
  maybe (return ()) (\_ ->
    lose
    ) lost
  return part
    { particlePosition = (nnx, nny)
    , particleTimeToLive = if (not $ null inters) then 0 else particleTimeToLive
    }

haskelloidShotDown :: Haskelloid -> Affection UserData ()
haskelloidShotDown h = do
  liftIO $ gegl_node_drop $ hNodeGraph h M.! "root"
  ud <- getAffection
  let redHaskelloids = delete h (haskelloids ud)
  liftIO $ gegl_node_drop $ hNodeGraph h M.! "root"
  newHaskelloids <- catMaybes <$> foldM
    (\acc _ ->
      if hDiv h < 4
      then
        liftIO $ insertHaskelloid acc (Just $ hDiv h) $ hPos h
      else
        return $ Nothing : acc
      )
    (map Just redHaskelloids) ([0..1] :: [Int])
  liftIO $ gegl_node_link_many $ map hFlange newHaskelloids
  if not $ null newHaskelloids
  then 
    liftIO $ gegl_node_link
      (last $ map hFlange newHaskelloids)
      (nodeGraph ud M.! KeyHNop)
  else do
    liftIO $ traceIO "YOU WON!"
    liftIO $ gegl_node_link
      (nodeGraph ud M.! KeyWon)
      (nodeGraph ud M.! KeyFGNop)
    putAffection ud
      { wonlost = True
      }
  putAffection ud
    { haskelloids = newHaskelloids
    }

updateHaskelloid :: Double -> Haskelloid -> Affection UserData Haskelloid
updateHaskelloid sec h@Haskelloid{..} = do
  let newX = (fst $ hPos) + sec * (fst $ hVel)
      newY = (snd $ hPos) + sec * (snd $ hVel)
      rawRot = hRot + hPitch * sec
      newRot
        | rawRot > 360  = rawRot - 360
        | rawRot < -360 = rawRot + 360
        | otherwise     = rawRot
      (nnx, nny) = wrapAround (newX, newY) (100 / fromIntegral hDiv)
  liftIO $ gegl_node_set (hNodeGraph M.! "trans") $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble $ nnx
    , Property "y" $ PropertyDouble $ nny
    ]
  liftIO $ gegl_node_set (hNodeGraph M.! "rot") $ Operation "gegl:rotate"
    [ Property "degrees" $ PropertyDouble newRot
    ]
  ud <- getAffection
  lost <- liftIO $ gegl_rectangle_intersect
    (GeglRectangle (floor nnx) (floor nny) (100 `div` hDiv) (100 `div` hDiv))
    (GeglRectangle
      (floor $ fst $ sPos $ ship ud)
      (floor $ snd $ sPos $ ship ud)
      50
      50
      )
  maybe (return ()) (\_ ->
    lose
    ) lost
  return h
    { hPos = (nnx, nny)
    , hRot = newRot
    }

lose :: Affection UserData ()
lose = do
  ud <- getAffection
  liftIO $ traceIO "YOU LOST!"
  _ <- liftIO $ gegl_node_link
    (nodeGraph ud M.! KeyLost)
    (nodeGraph ud M.! KeyFGNop)
  putAffection ud
    { wonlost = True
    }
  _ <- liftIO $ gegl_node_disconnect (nodeGraph ud M.! KeyShipOver) "aux"
  return ()
