{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Affection
import qualified SDL
import GEGL
import BABL

import qualified Data.Map as M

import Control.Monad (when)

import Debug.Trace

main :: IO ()
main = withAffection $ AffectionConfig
  { initComponents = All
  , windowTitle    = "Haskelloids"
  , windowConfig   = defaultWindow
  , preLoop        = return ()
  , drawLoop       = draw
  , updateLoop     = update
  , loadState      = load
  , cleanUp        = clean
  }

load :: SDL.Surface -> IO UserData
load _ = do
  traceM "loading"
  root <- gegl_node_new
  traceM "root node"
  ship <- gegl_node_new_child root $ Operation "gegl:svg-load"
    [ Property "path" $ PropertyString "assets/ship.svg"
    , Property "width" $ PropertyInt 50
    , Property "height" $ PropertyInt 50
    ]
  nop <- gegl_node_new_child root $ Operation "gegl:nop" []
  over <- gegl_node_new_child root $ defaultOverOperation
  translate <- gegl_node_new_child root $ Operation "gegl:translate"
    [ Property "origin-x" $ PropertyDouble 25
    , Property "origin-y" $ PropertyDouble 25
    , Property "x"        $ PropertyDouble 400
    , Property "y"        $ PropertyDouble 300
    , Property "sampler"  $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  rotate <- gegl_node_new_child root $ Operation "gegl:rotate"
    [ Property "origin-x" $ PropertyDouble 25
    , Property "origin-y" $ PropertyDouble 25
    , Property "sampler"  $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  crop <- gegl_node_new_child root $ Operation "gegl:crop"
    [ Property "width" $ PropertyDouble 800
    , Property "height" $ PropertyDouble 600
    ]
  buffer <- gegl_buffer_new (Just $ GeglRectangle 0 0 800 600) =<<
    babl_format (PixelFormat BABL.RGBA CFfloat)
  sink <- gegl_node_new_child root $ Operation "gegl:copy-buffer"
    [ Property "buffer" $ PropertyBuffer buffer
    ]
  gegl_node_link_many [ship, rotate, translate]
  gegl_node_link_many [over, crop, sink]
  gegl_node_connect_to translate "output" over "aux"
  traceM "nodes complete"
  myMap <- return $ M.fromList
    [ (KeyRoot, root)
    , (KeyTranslate, translate)
    , (KeyRotate, rotate)
    , (KeyShip, ship)
    , (KeyNop, nop)
    , (KeyCrop, crop)
    , (KeyOver, over)
    , (KeySink, sink)
    ]
  return $ UserData
    { nodeGraph = myMap
    , ship      = Ship
      { sPos = (400, 300)
      , sVel = (0, 0)
      , sRot = 0
      , sFlange = rotate
      }
    , buffer = buffer
    , shots = ParticleSystem (ParticleStorage Nothing []) over buffer
    }

data UserData = UserData
  { nodeGraph :: M.Map NodeKey GeglNode
  , ship :: Ship
  , buffer :: GeglBuffer
  -- , haskelloids :: ParticleSystem
  , shots :: ParticleSystem
  -- , debris :: ParticleSystem
  }

data Ship = Ship
  { sPos :: (Double, Double)
  , sVel :: (Double, Double)
  , sRot :: Double
  , sFlange :: GeglNode
  }

data NodeKey
  = KeyRoot
  | KeyTranslate
  | KeyRotate
  | KeyShip
  | KeyNop
  | KeyCrop
  | KeyOver
  | KeySink
  deriving (Ord, Eq)

update :: Double -> Affection UserData ()
update sec = do
  -- traceM $ (show $ 1 / sec) ++ " FPS"
  ad <- get
  evs <- SDL.pollEvents
  mapM_ (\e ->
    case SDL.eventPayload e of
      SDL.KeyboardEvent dat ->
        case (SDL.keysymKeycode $ SDL.keyboardEventKeysym dat) of
           SDL.KeycodeLeft -> do
             ud <- getAffection
             when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
               putAffection ud
                 { ship = (ship ud)
                   { sRot = (sRot $ ship ud) + 180 * sec
                   }
                 }
           SDL.KeycodeRight -> do
             ud <- getAffection
             when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
               putAffection ud
                 { ship = (ship ud)
                   { sRot = (sRot $ ship ud) - 180 * sec
                   }
                 }
           SDL.KeycodeUp ->
             when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $ do
               ud <- getAffection
               -- let vx = -10 * (sin (toR $ 2 * (sRot $ ship ud))) + fst (sVel $ ship ud)
               --     vy = -10 * (cos (toR $ 2 * (sRot $ ship ud))) + snd (sVel $ ship ud)
               let vx = -10 * (sin (toR $ (sRot $ ship ud))) + fst (sVel $ ship ud)
                   vy = -10 * (cos (toR $ (sRot $ ship ud))) + snd (sVel $ ship ud)
               putAffection ud
                 { ship = (ship ud)
                   { sVel = (vx, vy)
                   }
                 }
               traceM $ show (vx, vy) ++ " " ++ show ((*) 2 $ sRot $ ship ud)
           SDL.KeycodeSpace ->
             when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $ do
               ud <- getAffection
               ad <- get
               tempRoot <- liftIO $ gegl_node_new
               tempRect <- liftIO $ gegl_node_new_child tempRoot $ Operation "gegl:rectangle"
                 [ Property "x" $ (PropertyDouble $ (fst $ sPos $ ship ud) + 23)
                 , Property "y" $ (PropertyDouble $ (snd $ sPos $ ship ud) + 23)
                 , Property "width" $ PropertyDouble 4
                 , Property "height" $ PropertyDouble 4
                 , Property "color" $ PropertyColor $ (GEGL.RGBA 1 1 1 1)
                 ]
               tempOver <- liftIO $ gegl_node_new_child tempRoot $ defaultOverOperation
               liftIO $ gegl_node_connect_to tempRect "output" tempOver "aux"
               ips <- insertParticle (shots ud) $
                 Particle
                   { particleTimeToLive = 10
                   , particleCreation = elapsedTime ad
                   , particlePosition =
                     ( (fst $ sPos $ ship ud) + 23
                     , (snd $ sPos $ ship ud) + 23
                     )
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
                 }
                   -- XXX: Continue here
           _ -> return ()
      SDL.WindowClosedEvent _ -> do
        traceM "seeya!"
        put ad
          { quitEvent = True
          }
      _ -> return ()
    ) evs
  ud2 <- getAffection
  let nx = fst (sPos $ ship ud2) + (fst (sVel $ ship ud2)) * sec
      ny = snd (sPos $ ship ud2) + (snd (sVel $ ship ud2)) * sec
      -- nnx =
      --   if nx > 800
      --   then nx - 850
      --   else if nx < -50 then nx + 850 else nx
      -- nny =
      --   if ny > 600
      --   then ny - 650
      --   else if ny < -50 then ny + 650 else ny
      (nnx, nny) = wrapAround (nx, ny) 50
  liftIO $ gegl_node_set (nodeGraph ud2 M.! KeyTranslate) $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble $ nnx
    , Property "y" $ PropertyDouble $ nny
    ]
  liftIO $ gegl_node_set (nodeGraph ud2 M.! KeyRotate) $ Operation "gegl:rotate"
    [ Property "degrees" $ PropertyDouble $ sRot $ ship ud2
    ]
  ups <- updateParticleSystem (shots ud2) sec shotsUpd shotsDraw
  putAffection ud2
    { ship = (ship ud2)
      { sPos = (nnx, nny)
      }
    , shots = ups
    }

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
  traceM "drawing"
  ud <- getAffection
  liftIO $ gegl_node_process $ nodeGraph ud M.! KeySink
  -- mintr <- liftIO $ gegl_rectangle_intersect
  --   (GeglRectangle 0 0 800 600)
  --   (GeglRectangle (floor $ fst $ sPos $ ship ud) (floor $ snd $ sPos $ ship ud) 50 50)
  -- maybe (return ()) (\intr ->
  --   present
  --     (GeglRectangle (floor $ fst $ sPos $ ship ud) (floor $ snd $ sPos $ ship ud) 50 50)
  --     (buffer ud)
  --     False
  --   ) mintr
  -- XXX: above part crashes regularly for no apparent reason
  present
    (GeglRectangle 0 0 800 600)
    (buffer ud)
    False

clean :: UserData -> IO ()
clean ud = return ()

toR :: Double -> Double
toR deg = deg * pi / 180

shotsUpd :: Double -> Particle -> Affection UserData Particle
shotsUpd sec part@Particle{..} = do
  let newX = (fst particlePosition) + sec * (fromIntegral $ fst particleVelocity)
      newY = (snd particlePosition) + sec * (fromIntegral $ snd particleVelocity)
      (nnx, nny) = wrapAround (newX, newY) 2
  liftIO $ gegl_node_set (particleNodeGraph M.! "rect") $ Operation "gegl:rectangle"
    [ Property "x" $ (PropertyDouble $ nnx - 2)
    , Property "y" $ (PropertyDouble $ nny - 2)
    ]
  return part
    { particlePosition = (nnx, nny)
    }

shotsDraw :: GeglBuffer -> GeglNode -> Particle -> Affection UserData ()
shotsDraw buf node Particle{..} = do
  present
    (GeglRectangle (floor $ fst particlePosition - 2) (floor $ snd particlePosition - 2) 4 4)
    buf
    False
