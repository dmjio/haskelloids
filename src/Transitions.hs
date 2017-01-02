module Transitions where

import Affection
import qualified SDL
import GEGL
import BABL

loadMenu :: UserData -> IO UserData
loadMenu ud = do
  gegl_node_link (nodeGraph ud M.! KeyMenuOver) (nodeGraph ud M.! KeyFGNop)
  hs <- catMaybes <$> foldM (\acc _ -> do
    px <- randomRIO (0, 800)
    py <- randomRIO (0, 600)
    insertHaskelloid acc Nothing (px, py)
    ) [] ([0..9] :: [Int])
  liftIO $ gegl_node_link_many $ map hFlange hs
  liftIO $ gegl_node_link (last $ map hFlange hs) (nodeGraph ud M.! KeyHNop)
  return ud
    { haskelloids = hs
    , fade = FadeIn 1
    }
