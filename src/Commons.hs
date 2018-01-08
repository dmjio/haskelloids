module Commons where

import Affection

import Control.Monad.IO.Class (liftIO)

import System.Random (randomRIO)

import NanoVG as N hiding (V2(..))

import Linear as L hiding (rotate)

import Foreign.C.Types (CFloat(..))

import Types

toR :: Double -> Double
toR deg = deg * pi / 180

wrapAround :: (Fractional t, Ord t, Num t) => V2 t -> t -> V2 t
wrapAround (V2 nx ny) w = V2 nnx nny
  where
    nnx
      | nx > 800 + half = nx - (800 + w)
      | nx < -half      = nx + 800 + w
      | otherwise       = nx
    nny
      | ny > 600 + half = ny - (600 + w)
      | ny < -half      = ny + 600 + w
      | otherwise       = ny
    half = w / 2

newHaskelloids :: Affection UserData [Haskelloid]
newHaskelloids =
  do
  img <- haskImage <$> getAffection
  liftIO $ mapM (\_ -> do
    d <- randomRIO (1, 2)
    (posx, posy) <- getCoordinates d
    velx <- randomRIO (-10, 10)
    vely <- randomRIO (-10, 10)
    rot <- randomRIO (-180, 180)
    pitch <- randomRIO (-pi, pi)
    return $ Haskelloid
      (V2 posx posy)
      (V2 velx vely)
      rot
      pitch
      d
      img
    ) ([1..10] :: [Int])
  where
    getCoordinates d = do
      posx <- randomRIO (0, 800)
      posy <- randomRIO (0, 600)
      if distance (V2 posx posy) (V2 400 300) < 20 + (50 / fromIntegral d)
      then getCoordinates d
      else return (posx, posy)

updateHaskelloid :: Double -> Haskelloid -> Haskelloid
updateHaskelloid dsec has =
  has
    { hPos = wrapAround
      (hPos has + hVel has * V2 sec sec)
      (100 / fromIntegral (hDiv has))
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
  translate ctx (x + (w/2)) (y + (h/2))
  rotate ctx (degToRad $ CFloat rot)
  translate ctx (-(w/2)) (-(h/2))
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
drawHaskelloid (Haskelloid pos _ rot _ d img) = do
  ctx <- nano <$> getAffection
  liftIO $ drawImage ctx img (pos - fmap (/2) dim) dim rot 1
  where
    dim = fmap (/ fromIntegral d) (V2 100 100)
