{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Affection
import qualified SDL
import NanoVG

data UserData = UserData
  { ship :: Ship
  , haskelloids :: [Haskelloid]
  -- , shots :: ParticleSystem
  -- , debris :: ParticleSystem
  , wonlost :: Maybe WonLost
  , pixelSize :: Int
  , state :: State
  , fade :: MenuFade
  , neno :: Context
  }

data Ship = Ship
  { sPos :: V2 Double
  , sVel :: V2 Double
  , sRot :: Double
  , sImg :: Image
  }

data Haskelloid = Haskelloid
  { hPos :: V2 Double
  , hVel :: V2 Double
  , hRot :: Double
  , hPitch :: Double
  , hDiv :: Int
  , hImg :: Image
  } deriving (Eq)

data State
  = Menu
  | HighScore
  | InGame

data MenuFade
  = FadeIn Double
  | FadeOut Double

data WonLost
  = Won
  | Lost
  deriving (Eq)
