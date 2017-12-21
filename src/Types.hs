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
  , shots :: [Pew]
  -- , debris :: ParticleSystem
  , wonlost :: Maybe WonLost
  , pixelSize :: Int
  , state :: State
  , fade :: MenuFade
  , nano :: Context
  , font :: Font
  , subsystems :: Subsystems
  , haskImage :: Image
  , stateUUIDs :: UUIDClean
  }

data Ship = Ship
  { sPos :: V2 Float
  , sVel :: V2 Float
  , sRot :: Float
  , sImg :: Image
  }

data Pew = Pew
  { pPos :: V2 Float
  , pVel :: V2 Float
  , pTTL :: Double
  } deriving (Eq)

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
  -- | HighScore
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

data UUIDClean = UUIDClean
  { uuWindow   :: [MsgId WindowMessage]
  , uuKeyboard :: [MsgId KeyboardMessage]
  }

newtype Window = Window (TVar [(UUID, WindowMessage -> Affection UserData ())])

instance Participant Window WindowMessage UserData where
  partSubscribers (Window t) = do
    subTups <- liftIO $ readTVarIO t
    return $ map snd subTups

  partSubscribe (Window t) funct = do
    uuid <- genUUID
    liftIO $ atomically $ modifyTVar' t ((uuid, funct) :)
    return $ MsgId uuid MsgWindowEmptyEvent

  partUnSubscribe (Window t) (MsgId uuid _) =
    liftIO $ atomically $ modifyTVar' t (filter (flip filterMsg uuid))
    where
      filterMsg :: (UUID, WindowMessage -> Affection UserData ()) -> UUID -> Bool
      filterMsg (u, _) p = u /= p

instance SDLSubsystem Window UserData where
  consumeSDLEvents = consumeSDLWindowEvents

newtype Keyboard = Keyboard (TVar [(UUID, KeyboardMessage -> Affection UserData ())])

instance Participant Keyboard KeyboardMessage UserData where
  partSubscribers (Keyboard t) = do
    subTups <- liftIO $ readTVarIO t
    return $ map snd subTups

  partSubscribe (Keyboard t) funct = do
    uuid <- genUUID
    liftIO $ atomically $ modifyTVar' t ((uuid, funct) :)
    return $ MsgId uuid MsgKeyboardEmptyEvent

  partUnSubscribe (Keyboard t) (MsgId uuid _) =
    liftIO $ atomically $ modifyTVar' t (filter (flip filterMsg uuid))
    where
      filterMsg :: (UUID, KeyboardMessage -> Affection UserData ()) -> UUID -> Bool
      filterMsg (u, _) p = u /= p

instance SDLSubsystem Keyboard UserData where
  consumeSDLEvents = consumeSDLKeyboardEvents
