{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Affection
import qualified SDL
import NanoVG hiding (V2(..))
import Linear

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)

data UserData = UserData
  { ship :: Ship
  , haskelloids :: [Haskelloid]
  -- , shots :: ParticleSystem
  -- , debris :: ParticleSystem
  , wonlost :: Maybe WonLost
  , pixelSize :: Int
  , state :: State
  , fade :: MenuFade
  , nano :: Context
  , font :: Font
  , subsystems :: Subsystems
  , haskImage :: Image
  }

data Ship = Ship
  { sPos :: V2 Float
  , sVel :: V2 Float
  , sRot :: Float
  , sImg :: Image
  }

data Haskelloid = Haskelloid
  { hPos :: V2 Float
  , hVel :: V2 Float
  , hRot :: Float
  , hPitch :: Float
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

data Subsystems = Subsystems
  { subWindow :: Window
  , subKeyboard :: Keyboard
  }

newtype Window = Window (TVar [(UUID, WindowMessage -> Affection UserData ())])

instance Participant Window WindowMessage UserData where
  partSubscribers (Window t) = do
    subTups <- liftIO $ readTVarIO t
    return $ map snd subTups

  partSubscribe (Window t) = generalSubscribe t

  partUnSubscribe (Window t) uuid =
    liftIO $ atomically $ modifyTVar' t (filter (\(u, _) -> u /= uuid))

instance SDLSubsystem Window UserData where
  consumeSDLEvents = consumeSDLWindowEvents

newtype Keyboard = Keyboard (TVar [(UUID, KeyboardMessage -> Affection UserData ())])

instance Participant Keyboard KeyboardMessage UserData where
  partSubscribers (Keyboard t) = do
    subTups <- liftIO $ readTVarIO t
    return $ map snd subTups

  partSubscribe (Keyboard t) = generalSubscribe t

  partUnSubscribe (Keyboard t) uuid =
    liftIO $ atomically $ modifyTVar' t (filter (\(u, _) -> u /= uuid))

instance SDLSubsystem Keyboard UserData where
  consumeSDLEvents = consumeSDLKeyboardEvents

generalSubscribe t funct = do
  uuid <- genUUID
  liftIO $ atomically $ modifyTVar' t ((uuid, funct) :)
  return uuid
