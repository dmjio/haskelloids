module Init where

import Affection

import SDL (($=))
import qualified SDL

import qualified Data.Set as S
import Data.Maybe

import System.Random

import NanoVG

-- Internal imports

import Types

load :: IO UserData
load = do
  nvgCtx <- createGL3 (S.fromList [Antialias, StencilStrokes])
  mhaskImage <- createImage nvgCtx "assets/haskelloid.svg" 0
  mshipImage <- createImage nvgCtx "assets/ship.svg" 0
  when (isNothing mhasImage || isNothing mshipImage) $
    logIO Error "Failed loading image assets"
  hasks <- mapM (\_ -> do
    posx <- randomRIO (0, 800)
    posy <- randomRIO (0, 600)
    velx <- randomRIO (-10, 10)
    vely <- randomRIO (-10, 10)
    rot <- randomRIO (0, 2*pi)
    pitch <- randomRIO (-pi, pi)
    div <- randomRIO (1, 2)
    return Haskelloid
      (V2 posx posy)
      (V2 velx vely)
      rot
      pitch
      div
      (fromJust mhaskImage)
    ) [1..10]
  return UserData
    { ship = Ship
      { sPos = V2 400 300
      , sVel = V2 0 0
      , sRot = 0
      , sImg = fromust mshipImage
      }
    , haskelloids = []
    , wonlost = Nothing
    , state = Menu
    , fade = FadeIn 1
    , nano = nvgCtx
    }



  -- _ <- SDL.setMouseLocationMode SDL.RelativeLocation
  -- GL.depthFunc $= ust GL.Less
  -- pane <- GL.genObjectName
  -- GL.BindVertexArrayObject $= Just pane
  -- verts <- GL.genObejctName
  -- let vertCoord =
  --       [ (-1), (-1), 0
  --       , 1   , (-1), 0
  --       , (-1), 1   , 0
  --       , 1   , 1   , 0
  --       , (-1), 1   , 0
  --       , 1   , (-1), 0
  --       ]
  -- withArray vertCoord $ \ptr
  --   GL.bufferData GL.ArrayBuffer $=
  --     ( fromIntegral $ length vertCoord * 3 * sizeOf (0 :: Double)
  --     , ptr
  --     , GL.StaticDraw
  --     )
  -- GL.vertexAttribPointer (GL.AttribLocation 0) $=
  --   ( GL.ToFloat
  --   , GL.VertexArrayDescriptor 4 GL.Float 0 (plusPtr nullPtr 0)
  --   )
 -- GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
