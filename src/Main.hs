{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Affection
import qualified SDL
import GEGL
import BABL

import Data.List (delete)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Control.Monad (when, foldM)

import System.Random (randomRIO)

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
  bg <- gegl_node_new_child root $ Operation "gegl:rectangle"
    [ Property "x" $ PropertyDouble 0
    , Property "y" $ PropertyDouble 0
    , Property "width" $ PropertyDouble 800
    , Property "height" $ PropertyDouble 600
    , Property "color" $ PropertyColor $ GEGL.RGBA 0 0 0.1 1
    ]
  ship <- gegl_node_new_child root $ Operation "gegl:svg-load"
    [ Property "path" $ PropertyString "assets/ship.svg"
    , Property "width" $ PropertyInt 50
    , Property "height" $ PropertyInt 50
    ]
  pnop <- gegl_node_new_child root $ Operation "gegl:nop" []
  hnop <- gegl_node_new_child root $ Operation "gegl:nop" []
  sover <- gegl_node_new_child root $ defaultOverOperation
  hover <- gegl_node_new_child root $ defaultOverOperation
  pover <- gegl_node_new_child root $ defaultOverOperation
  bgover <- gegl_node_new_child root $ defaultOverOperation
  fgover <- gegl_node_new_child root $ defaultOverOperation
  translate <- gegl_node_new_child root $ Operation "gegl:translate"
    [ Property "x"        $ PropertyDouble 375
    , Property "y"        $ PropertyDouble 275
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
  won <- gegl_node_new_child root $ textOperation
    [ Property "string" $ PropertyString "YOU WON!"
    , Property "size"   $ PropertyDouble 100
    , Property "color"  $ PropertyColor $ GEGL.RGBA 1 1 1 1
    ]
  lost <- gegl_node_new_child root $ textOperation
    [ Property "string" $ PropertyString "YOU LOST!"
    , Property "size"   $ PropertyDouble 100
    , Property "color"  $ PropertyColor $ GEGL.RGBA 1 1 1 1
    ]
  vignette <- gegl_node_new_child root $ Operation "gegl:vignette" []
  pixelize <- gegl_node_new_child root $ Operation "gegl:pixelize"
    [ Property "size-x" $ PropertyInt 3
    , Property "size-y" $ PropertyInt 3
    ]
  -- vdeg <- gegl_node_new_child root $ Operation "gegl:video-degradation"
  --   [ Property "pattern" $ PropertyInt 8
  --   ]
  gegl_node_link_many [ship, rotate, translate]
  gegl_node_link_many [bgover, pover, hover, sover, crop, fgover, pixelize, vignette, sink]
  _ <- gegl_node_connect_to translate "output" sover "aux"
  _ <- gegl_node_connect_to pnop "output" pover "aux"
  _ <- gegl_node_connect_to hnop "output" hover "aux"
  _ <- gegl_node_connect_to bg "output" bgover "aux"
  traceM "nodes complete"
  myMap <- return $ M.fromList
    [ (KeyRoot, root)
    , (KeyTranslate, translate)
    , (KeyRotate, rotate)
    , (KeyShip, ship)
    , (KeyPNop, pnop)
    , (KeyHNop, hnop)
    , (KeyCrop, crop)
    , (KeyShipOver, sover)
    , (KeySink, sink)
    , (KeyWon, won)
    , (KeyLost, lost)
    , (KeyPixelize, pixelize)
    , (KeyFGOver, fgover)
    ]
  hs <- catMaybes <$> foldM (\acc _ -> do
    px <- liftIO $ randomRIO (0, 800)
    py <- liftIO $ randomRIO (0, 600)
    insertHaskelloid acc Nothing (px, py)
    ) [] ([0..9] :: [Int])
  liftIO $ gegl_node_link_many $ map hFlange hs
  liftIO $ gegl_node_link (last $ map hFlange hs) hnop
  return $ UserData
    { nodeGraph = myMap
    , ship      = Ship
      { sPos = (375, 275)
      , sVel = (0, 0)
      , sRot = 0
      , sFlange = rotate
      }
    , buffer = buffer
    , shots = ParticleSystem (ParticleStorage Nothing []) pnop buffer
    , haskelloids = hs
    , wonlost = False
    , pixelSize = 3
    }

data UserData = UserData
  { nodeGraph :: M.Map NodeKey GeglNode
  , ship :: Ship
  , buffer :: GeglBuffer
  , haskelloids :: [Haskelloid]
  , shots :: ParticleSystem
  -- , debris :: ParticleSystem
  , wonlost :: Bool
  , pixelSize :: Int
  }

data Ship = Ship
  { sPos :: (Double, Double)
  , sVel :: (Double, Double)
  , sRot :: Double
  , sFlange :: GeglNode
  }

data Haskelloid = Haskelloid
  { hPos :: (Double, Double)
  , hVel :: (Double, Double)
  , hRot :: Double
  , hPitch :: Double
  , hDiv :: Int
  , hFlange :: GeglNode
  , hNodeGraph :: M.Map String GeglNode
  } deriving (Eq)

data NodeKey
  = KeyRoot
  | KeyTranslate
  | KeyRotate
  | KeyShip
  | KeyPNop
  | KeyHNop
  | KeyCrop
  | KeyShipOver
  | KeySink
  | KeyWon
  | KeyLost
  | KeyPixelize
  | KeyFGOver
  deriving (Ord, Eq)

update :: Double -> Affection UserData ()
update sec = do
  -- traceM $ (show $ 1 / sec) ++ " FPS"
  ad <- get
  evs <- SDL.pollEvents
  wd <- getAffection
  when (((floor $ elapsedTime ad) * 100) `mod` 10 < 2 && pixelSize wd > 3) $ do
    liftIO $ gegl_node_set (nodeGraph wd M.! KeyPixelize) $ Operation "gegl:pixelize"
      [ Property "size-x" $ PropertyInt $ pixelSize wd - 1
      , Property "size-y" $ PropertyInt $ pixelSize wd - 1
      ]
    pd <- getAffection
    putAffection pd
      { pixelSize = pixelSize wd -1
      }
  mapM_ (\e ->
    case SDL.eventPayload e of
      SDL.KeyboardEvent dat ->
        case (SDL.keysymKeycode $ SDL.keyboardEventKeysym dat) of
           SDL.KeycodeLeft -> do
             ud <- getAffection
             when (SDL.keyboardEventKeyMotion dat == SDL.Pressed && not (wonlost wd)) $
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
               let posX = (fst $ sPos $ ship ud) + 23 - 30 * sin (toR $ sRot $ ship ud)
                   posY = (snd $ sPos $ ship ud) + 23 - 30 * cos (toR $ sRot $ ship ud)
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
           _ -> return ()
      SDL.WindowClosedEvent _ -> do
        traceM "seeya!"
        put ad
          { quitEvent = True
          }
      _ -> return ()
    ) evs
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
  ups <- updateParticleSystem (shots ud3) sec shotsUpd shotsDraw
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
  -- traceM "drawing"
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
    (not $ wonlost ud)

clean :: UserData -> IO ()
clean ud = gegl_node_drop $ nodeGraph ud M.! KeyRoot

toR :: Double -> Double
toR deg = deg * pi / 180

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
  killings <- mapM haskelloidShotDown inters
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
    , particleTimeToLive = if (not $ null killings) then 0 else particleTimeToLive
    }

haskelloidShotDown :: Haskelloid -> Affection UserData ()
haskelloidShotDown h = do
  -- liftIO $ traceIO "Haskelloid shot down"
  liftIO $ gegl_node_drop $ hNodeGraph h M.! "root"
  ud <- getAffection
  let redHaskelloids = delete h (haskelloids ud)
  newHaskelloids <- catMaybes <$> foldM
    (\acc _ ->
      if hDiv h < 4
      then
        liftIO $ insertHaskelloid acc (Just $ hDiv h) $ hPos h
      else
        return $ Nothing : acc
      )
    (map Just redHaskelloids) ([0..1] :: [Int])
  -- liftIO $ traceIO $ show $ length newHaskelloids
  liftIO $ gegl_node_link_many $ map hFlange newHaskelloids
  if not $ null newHaskelloids
  then 
    liftIO $ gegl_node_link
      (last $ map hFlange newHaskelloids)
      (nodeGraph ud M.! KeyHNop)
  else do
    liftIO $ traceIO "YOU WON!"
    _ <- liftIO $ gegl_node_connect_to
      (nodeGraph ud M.! KeyWon)
      "output"
      (nodeGraph ud M.! KeyFGOver)
      "aux"
    putAffection ud
      { wonlost = True
      }
  putAffection ud
    { haskelloids = newHaskelloids
    }

shotsDraw :: GeglBuffer -> GeglNode -> Particle -> Affection UserData ()
shotsDraw _ _ _ = return ()

updateHaskelloid :: Double -> Haskelloid -> Affection UserData Haskelloid
updateHaskelloid sec h@Haskelloid{..} = do
  let newX = (fst $ hPos) + sec * (fst $ hVel)
      newY = (snd $ hPos) + sec * (snd $ hVel)
      newRot = hRot + hPitch * sec
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

insertHaskelloid :: [Maybe Haskelloid] -> Maybe Int -> (Double, Double) -> IO [Maybe Haskelloid]
insertHaskelloid hs split (px, py) = do
  -- liftIO $ traceIO "inserting haskelloid"
  vx <- liftIO $ randomRIO (-10, 10)
  vy <- liftIO $ randomRIO (-10, 10)
  rdiv <- case split of
    Nothing -> liftIO $ randomRIO (1, 2)
    Just x -> return $ x + 1
  rot <- liftIO $ randomRIO (0, 360)
  pitch <- liftIO $ randomRIO (-45, 45)
  tempRoot <- liftIO $ gegl_node_new
  tempOver <- liftIO $ gegl_node_new_child tempRoot $ defaultOverOperation
  tempSvg <- gegl_node_new_child tempRoot $ Operation "gegl:svg-load"
    [ Property "path" $ PropertyString "assets/haskelloid.svg"
    , Property "width" $ PropertyInt (100 `div` rdiv)
    , Property "height" $ PropertyInt (100 `div` rdiv)
    ]
  tempTrans <- liftIO $ gegl_node_new_child tempRoot $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble px
    , Property "y" $ PropertyDouble py
    , Property "sampler"  $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  tempRot <- liftIO $ gegl_node_new_child tempRoot $ Operation "gegl:rotate"
    [ Property "origin-x" $ PropertyDouble (100 / 2 / fromIntegral rdiv)
    , Property "origin-y" $ PropertyDouble (100 / 2 / fromIntegral rdiv)
    , Property "degrees" $ PropertyDouble rot
    , Property "sampler"  $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  liftIO $ gegl_node_link_many [tempSvg, tempRot, tempTrans]
  _ <- liftIO $ gegl_node_connect_to tempTrans "output" tempOver "aux"
  return $ Just  Haskelloid
    { hPos = (px, py)
    , hVel = (vx, vy)
    , hRot = rot
    , hPitch = pitch
    , hDiv = rdiv
    , hFlange = tempOver
    , hNodeGraph = M.fromList
      [ ("root", tempRoot)
      , ("over", tempOver)
      , ("svg", tempSvg)
      , ("trans", tempTrans)
      , ("rot", tempRot)
      ]
    } : hs

lose :: Affection UserData ()
lose = do
  ud <- getAffection
  liftIO $ traceIO "YOU LOST!"
  _ <- liftIO $ gegl_node_connect_to
    (nodeGraph ud M.! KeyLost)
    "output"
    (nodeGraph ud M.! KeyFGOver)
    "aux"
  putAffection ud
    { wonlost = True
    }
  liftIO $ gegl_node_drop $ nodeGraph ud M.! KeyShip
