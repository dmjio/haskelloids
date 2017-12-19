{-# LANGUAGE RecordWildCards #-}

module Commons where

import Affection
import qualified SDL

import qualified Data.Map as M
import Data.List (delete)
import Data.Maybe (catMaybes, isJust)

import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (liftIO)

import System.Random (randomRIO)

import NanoVG hiding (V2(..))

import Linear

import Types

toR :: Double -> Double
toR deg = deg * pi / 180

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

newHaskelloids :: Image -> Affection UserData [Haskelloid]
newHaskelloids img = liftIO $ mapM (\_ -> do
  posx <- randomRIO (0, 800)
  posy <- randomRIO (0, 600)
  velx <- randomRIO (-10, 10)
  vely <- randomRIO (-10, 10)
  rot <- randomRIO (0, 2*pi)
  pitch <- randomRIO (-pi, pi)
  div <- randomRIO (1, 2)
  return $ Haskelloid
    (V2 posx posy)
    (V2 velx vely)
    rot
    pitch
    div
    img
  ) [1..10]

updateHaskelloid :: Double -> Haskelloid -> Haskelloid
updateHaskelloid sec has =
  has
    { hPos = hPos has + hVel has * V2 sec sec
    , hRot = hRot has + hPitch has * sec
    }

clamp :: Ord a => a -> a -> a -> a
clamp a' low up
  | a' < low = low
  | a' > up = up
  | otherwise = a'
