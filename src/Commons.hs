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

import NanoVG as N hiding (V2(..))

import Linear as L hiding (rotate)

import Foreign.C.Types (CFloat(..))

import Types

toR :: Double -> Double
toR deg = deg * pi / 180

wrapAround :: (Ord t, Num t) => V2 t -> t -> V2 t
wrapAround (V2 nx ny) width = (V2 nnx nny)
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
  rot <- randomRIO (-180, 180)
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
updateHaskelloid dsec has =
  has
    { hPos = wrapAround (hPos has + hVel has * V2 sec sec) (100 / fromIntegral (hDiv has))
    , hRot = hRot has + hPitch has * sec
    }
  where
    sec = realToFrac dsec

clamp :: Ord a => a -> a -> a -> a
clamp a' low up
  | a' < low = low
  | a' > up = up
  | otherwise = a'

drawImage :: Context -> Image -> V2 Float -> V2 Float -> Float -> Float -> IO ()
drawImage ctx img pos dim rot alpha = do
  let (V2 x y) = fmap CFloat pos
      (V2 w h) = fmap CFloat dim
  save ctx
  translate ctx x y
  rotate ctx (degToRad $ CFloat rot)
  sPaint <- imagePattern ctx 0 0 w h 0 img (CFloat alpha)
  beginPath ctx
  rect ctx 0 0 w h
  fillPaint ctx sPaint
  fill ctx
  resetTransform ctx
  restore ctx

drawSpinner :: Context -> Float -> Float -> Float -> Float -> IO ()
drawSpinner ctx x y cr ct = do
  let a0 = 0+t*6
      a1 = pi + t*6
      r0 = r
      r1 = r*0.75
      (cx, cy) = (CFloat x, CFloat y)
      r = CFloat cr
      t = CFloat ct
  save ctx
  beginPath ctx
  arc ctx cx cy r0 a0 a1 CW
  arc ctx cx cy r1 a1 a0 CCW
  closePath ctx
  let ax = cx+cos a0 * (r0+r1)*0.5
      ay = cy+sin a0 * (r0+r1)*0.5
      bx = cx+cos a1 * (r0+r1)*0.5
      by = cy+sin a1 * (r0+r1)*0.5
  paint <- linearGradient ctx ax ay bx by (rgba 255 255 255 0) (rgba 255 255 255 128)
  fillPaint ctx paint
  fill ctx
  restore ctx

drawHaskelloid :: Haskelloid -> Affection UserData ()
drawHaskelloid (Haskelloid pos _ rot _ div img) = do
  ctx <- nano <$> getAffection
  liftIO $ drawImage ctx img pos (fmap (/ fromIntegral div) (V2 100 100)) rot 255
