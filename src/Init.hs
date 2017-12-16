module Init where

import Affection as A

import SDL (($=))
import qualified SDL

import qualified Data.Set as S
import Data.Maybe

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import System.Random

import NanoVG hiding (V2(..))

import Linear

-- Internal imports

import Types

load :: IO UserData
load = do
  liftIO $ logIO A.Debug "loading state"
  liftIO $ logIO A.Debug "creating NanoVG context"
  nvgCtx <- createGL3 (S.fromList [Antialias, StencilStrokes])
  liftIO $ logIO A.Debug "load ship image"
  mshipImage <- createImage nvgCtx (FileName "assets/ship.svg") 0
  when (isNothing mshipImage) $
    logIO Error "Failed loading image assets"
  return UserData
    { ship = Ship
      { sPos = V2 400 300
      , sVel = V2 0 0
      , sRot = 0
      , sImg = fromJust mshipImage
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
