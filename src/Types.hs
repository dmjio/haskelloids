module Types where

import Affection hiding (StateMachine)
import qualified SDL
import GEGL
import BABL
import qualified Data.Map as M

data UserData = UserData
  { nodeGraph :: M.Map NodeKey GeglNode
  , ship :: Ship
  , buffer :: GeglBuffer
  , haskelloids :: [Haskelloid]
  , shots :: ParticleSystem
  -- , debris :: ParticleSystem
  , wonlost :: Bool
  , pixelSize :: Int
  , state :: StateMachine
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
  | KeyFGNop
  deriving (Ord, Eq)

data StateMachine
  = Menu
  | HighScore
  | InGame
