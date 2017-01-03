{-# LANGUAGE RecordWildCards #-}

module Commons where

import Affection
import qualified SDL
import GEGL
import BABL

import qualified Data.Map as M
import Data.List (delete)
import Data.Maybe (catMaybes)

import Control.Monad (foldM, unless)

import System.Random (randomRIO)

import Debug.Trace

import Types

toR :: Double -> Double
toR deg = deg * pi / 180

clean :: UserData -> IO ()
clean ud = do
  mapM_ (gegl_node_drop . (\h -> hNodeGraph h M.! "root")) (haskelloids ud)
  gegl_node_drop $ nodeGraph ud M.! KeyRoot

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
  shipNode <- gegl_node_new_child root $ Operation "gegl:svg-load"
    [ Property "path" $ PropertyString "assets/ship.svg"
    , Property "width" $ PropertyInt 50
    , Property "height" $ PropertyInt 50
    ]
  pnop <- gegl_node_new_child root $ Operation "gegl:nop" []
  hnop <- gegl_node_new_child root $ Operation "gegl:nop" []
  fgnop <- gegl_node_new_child root $ Operation "gegl:nop" []
  sover <- gegl_node_new_child root defaultOverOperation
  hover <- gegl_node_new_child root defaultOverOperation
  pover <- gegl_node_new_child root defaultOverOperation
  bgover <- gegl_node_new_child root defaultOverOperation
  fgover <- gegl_node_new_child root defaultOverOperation
  translate <- gegl_node_new_child root $ Operation "gegl:translate"
    [ Property "x"        $ PropertyDouble 375
    , Property "y"        $ PropertyDouble 275
    , Property "sampler"  $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  fgtranslate <- gegl_node_new_child root $ Operation "gegl:translate"
    [ Property "x"        $ PropertyDouble 150
    , Property "y"        $ PropertyDouble 250
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
  nbuffer <- gegl_buffer_new (Just $ GeglRectangle 0 0 800 600) =<<
    babl_format (PixelFormat BABL.RGBA CFfloat)
  sink <- gegl_node_new_child root $ Operation "gegl:copy-buffer"
    [ Property "buffer" $ PropertyBuffer nbuffer
    ]
  won <- gegl_node_new_child root $ textOperation
    [ Property "string" $ PropertyString "YOU WON!"
    , Property "font"   $ PropertyString "Modulo"
    , Property "size"   $ PropertyDouble 100
    , Property "color"  $ PropertyColor $ GEGL.RGBA 1 1 1 1
    ]
  lost <- gegl_node_new_child root $ textOperation
    [ Property "string" $ PropertyString "YOU LOST!"
    , Property "font"   $ PropertyString "Modulo"
    , Property "size"   $ PropertyDouble 100
    , Property "color"  $ PropertyColor $ GEGL.RGBA 1 1 1 1
    ]
  vignette <- gegl_node_new_child root $ Operation "gegl:vignette" []
  -- pixelize <- gegl_node_new_child root $ Operation "gegl:pixelize"
  pixelize <- gegl_node_new_child root $ Operation "gegl:pixelize"
    [ Property "size-x" $ PropertyInt 3
    , Property "size-y" $ PropertyInt 3
    ]
  -- vdeg <- gegl_node_new_child root $ Operation "gegl:video-degradation"
  --   [ Property "pattern" $ PropertyInt 8
  --   ]
  menuHeading <- gegl_node_new_child root $ textOperation
    [ Property "string" $ PropertyString "Haskelloids"
    , Property "font"   $ PropertyString "Modulo"
    , Property "size"   $ PropertyDouble 100
    , Property "color"  $ PropertyColor $ GEGL.RGBA 1 1 1 1
    ]
  menuText <- gegl_node_new_child root $ textOperation
    [ Property "string" $ PropertyString "Press [Space] to start\nPress [H] for Highscore"
    , Property "font"   $ PropertyString "Modulo"
    , Property "size"   $ PropertyDouble 50
    , Property "alignment" $ PropertyInt 1
    , Property "color"  $ PropertyColor $ GEGL.RGBA 1 1 1 1
    ]
  menuTranslateHeading <- gegl_node_new_child root $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble 150
    , Property "y" $ PropertyDouble 100
    , Property "sampler" $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  menuTranslateText <- gegl_node_new_child root $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble 130
    , Property "y" $ PropertyDouble 300
    , Property "sampler" $ PropertyInt $ fromEnum GeglSamplerCubic
    ]
  menuOver <- gegl_node_new_child root defaultOverOperation
  gegl_node_link menuHeading menuTranslateHeading
  gegl_node_link_many [menuText, menuTranslateText, menuOver]
  _ <- gegl_node_connect_to menuTranslateHeading "output" menuOver "aux"
  gegl_node_link_many [shipNode, rotate, translate]
  gegl_node_link_many [bgover, pover, hover, sover, crop, fgover, pixelize, vignette, sink]
  -- _ <- gegl_node_connect_to translate "output" sover "aux"
  _ <- gegl_node_connect_to pnop "output" pover "aux"
  _ <- gegl_node_connect_to hnop "output" hover "aux"
  _ <- gegl_node_connect_to bg "output" bgover "aux"
  -- liftIO $ gegl_node_link fgnop fgtranslate
  _ <- gegl_node_connect_to fgtranslate "output" fgover "aux"
  _ <- gegl_node_connect_to fgnop "output" fgover "aux"
  traceM "nodes complete"
  myMap <- return $ M.fromList
    [ (KeyRoot, root)
    , (KeyShip, shipNode)
    , (KeyShipTranslate, translate)
    , (KeyShipRotate, rotate)
    , (KeyPNop, pnop)
    , (KeyHNop, hnop)
    , (KeyCrop, crop)
    , (KeyShipOver, sover)
    , (KeySink, sink)
    , (KeyWon, won)
    , (KeyLost, lost)
    , (KeyPixelize, pixelize)
    , (KeyFGOver, fgover)
    , (KeyFGNop, fgnop)
    , (KeyMenuHeading, menuTranslateHeading)
    , (KeyMenuText, menuText)
    , (KeyMenuOver, menuOver)
    ]
  -- hs <- catMaybes <$> foldM (\acc _ -> do
  --   px <- liftIO $ randomRIO (0, 800)
  --   py <- liftIO $ randomRIO (0, 600)
  --   insertHaskelloid acc Nothing (px, py)
  --   ) [] ([0..9] :: [Int])
  -- liftIO $ gegl_node_link_many $ map hFlange hs
  -- liftIO $ gegl_node_link (last $ map hFlange hs) hnop
  return UserData
    { nodeGraph = myMap
    , ship      = Ship
      { sPos = (375, 275)
      , sVel = (0, 0)
      , sRot = 0
      , sFlange = rotate
      }
    , buffer = nbuffer
    , shots = ParticleSystem (ParticleStorage Nothing []) pnop nbuffer
    -- , haskelloids = hs
    , haskelloids = []
    , wonlost = False
    , pixelSize = 3
    , state = Menu
    , fade = FadeIn 1
    }

insertHaskelloid :: [Maybe Haskelloid] -> Maybe Int -> (Double, Double) -> IO [Maybe Haskelloid]
insertHaskelloid hasks split (px, py) = do
  -- liftIO $ traceIO "inserting haskelloid"
  vx <- liftIO $ randomRIO (-10, 10)
  vy <- liftIO $ randomRIO (-10, 10)
  rdiv <- case split of
    Nothing -> liftIO $ randomRIO (1, 2)
    Just x -> return $ x + 1
  rot <- liftIO $ randomRIO (0, 360)
  pitch <- liftIO $ randomRIO (-45, 45)
  tempRoot <- liftIO gegl_node_new
  tempOver <- liftIO $ gegl_node_new_child tempRoot defaultOverOperation
  tempSvg <- gegl_node_new_child tempRoot $ Operation "gegl:svg-load"
    [ Property "path" $ PropertyString "assets/haskelloid.svg"
    , Property "width" $ PropertyInt (100 `div` rdiv)
    , Property "height" $ PropertyInt (100 `div` rdiv)
    ]
  tempTrans <- liftIO $ gegl_node_new_child tempRoot $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble $ px + (100 / fromIntegral rdiv / 2)
    , Property "y" $ PropertyDouble $ py + (100 / fromIntegral rdiv / 2)
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
    { hPos =
      ( px + (100 / 2 / fromIntegral rdiv)
      , py + (100 / 2 / fromIntegral rdiv)
      )
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
    } : hasks

haskelloidShotDown :: Haskelloid -> Affection UserData ()
haskelloidShotDown h = do
  ud <- getAffection
  -- liftIO $ traceIO $ show $ length $ haskelloids ud
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
  liftIO $ gegl_node_drop $ hNodeGraph h M.! "root"
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
  let newX = fst hPos + sec * fst hVel
      newY = snd hPos + sec * snd hVel
      newRot = hRot + hPitch * sec
      (nnx, nny) = wrapAround (newX, newY) (100 / fromIntegral hDiv)
  -- liftIO $ traceIO $ "moving to: " ++ show nnx ++ " " ++ show nny
  liftIO $ gegl_node_set (hNodeGraph M.! "trans") $ Operation "gegl:translate"
    [ Property "x" $ PropertyDouble nnx
    , Property "y" $ PropertyDouble nny
    ]
  liftIO $ gegl_node_set (hNodeGraph M.! "rot") $ Operation "gegl:rotate"
    [ Property "degrees" $ PropertyDouble newRot
    ]
  ud <- getAffection
  lost <- 
    case state ud of
      InGame -> liftIO $ gegl_rectangle_intersect
        (GeglRectangle (floor nnx) (floor nny) (100 `div` hDiv) (100 `div` hDiv))
        (GeglRectangle
          (floor $ fst $ sPos $ ship ud)
          (floor $ snd $ sPos $ ship ud)
          50
          50
          )
      _ -> return Nothing
  maybe (return ()) (const lose) lost
  return h
    { hPos = (nnx, nny)
    , hRot = newRot
    }

wrapAround :: (Ord t, Num t) => (t, t) -> t -> (t, t)
wrapAround (nx, ny) width = (nnx, nny)
  where
    nnx
      | nx > 800    = nx - (800 + width)
      | nx < -width = nx + 800 + width
      | otherwise   = nx
    nny
      | ny > 600    = ny - (600 + width)
      | ny < -width = ny + 600 + width
      | otherwise   = ny

shotsUpd :: Double -> Particle -> Affection UserData Particle
shotsUpd sec part@Particle{..} = do
  let newX = fst particlePosition + sec * fromIntegral (fst particleVelocity)
      newY = snd particlePosition + sec * fromIntegral (snd particleVelocity)
      (nnx, nny) = wrapAround (newX, newY) 4
  liftIO $ gegl_node_set (particleNodeGraph M.! "rect") $ Operation "gegl:rectangle"
    [ Property "x" $ PropertyDouble nnx
    , Property "y" $ PropertyDouble nny
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
  unless (null inters) $
    haskelloidShotDown $ head inters
  lost <- liftIO $ gegl_rectangle_intersect
    (GeglRectangle (floor nnx) (floor nny) 4 4)
    (GeglRectangle
      (floor $ fst $ sPos $ ship ud)
      (floor $ snd $ sPos $ ship ud)
      50
      50
      )
  maybe (return ()) (const lose) lost
  return part
    { particlePosition = (nnx, nny)
    , particleTimeToLive = if not $ null inters then 0 else particleTimeToLive
    }

shotsDraw :: GeglBuffer -> GeglNode -> Particle -> Affection UserData ()
shotsDraw _ _ _ = return ()

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
