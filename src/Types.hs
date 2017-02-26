{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Affection
import qualified SDL
import GEGL
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
  , state :: State
  , fade :: MenuFade
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
  | KeyShip
  | KeyShipTranslate
  | KeyShipRotate
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
  | KeyFGTrans
  | KeyMenuHeading
  | KeyMenuText
  | KeyMenuStart
  | KeyMenuHighscore
  | KeyMenuOver
  deriving (Ord, Eq)

data State
  = Menu
  | HighScore
  | InGame

data MenuFade
  = FadeIn Double
  | FadeOut Double
