module Menu where

import Affection
import qualified SDL

import Debug.Trace

import Types

handleMenuEvent :: SDL.EventPayload -> Affection UserData ()
handleMenuEvent e =
  case e of
    SDL.KeyboardEvent dat ->
      case SDL.keysymKeycode $ SDL.keyboardEventKeysym dat of
        SDL.KeycodeSpace ->
          when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $ do
            ud <- getAffection
            liftIO $ gegl_node_disconnect (nodeGraph ud M.! KeyFGNop) "input"
            smLoad InGame
        _ -> return ()
    SDL.WindowClosedEvent _ -> do
      ad <- get
      put ad
        { quitEvent = True
        }
    _ -> return ()

loadMenu :: Affection UserData ()
loadMenu = do
  ud <- getAffection
  liftIO $ gegl_node_connect_to
    (nodeGraph ud M.! KeyMenuOver)
    "output"
    (nodeGraph ud M.! KeyFGOver)
    "aux"
  hs <- liftIO $ catMaybes <$> foldM (\acc _ -> do
    px <- randomRIO (0, 800)
    py <- randomRIO (0, 600)
    insertHaskelloid acc Nothing (px, py)
    ) [] ([0..9] :: [Int])
  liftIO $ gegl_node_link_many $ map hFlange hs
  liftIO $ gegl_node_link (last $ map hFlange hs) (nodeGraph ud M.! KeyHNop)
  -- liftIO $ gegl_node_disconnect (nodeGraph ud M.! KeyPNop) "input"
  putAffection ud
    { haskelloids = hs
    , fade = FadeIn 1
    , state = Menu
    , shots = (shots ud)
      { partSysParts = ParticleStorage Nothing [] }
    }

updateMenu :: Double -> Affection UserData ()
updateMenu sec = do
  ud <- getAffection
  nhs <- mapM (updateHaskelloid sec) (haskelloids ud)
  case fade ud of
    FadeIn ttl -> do
      liftIO $ gegl_node_set (nodeGraph ud M.! KeyMenuText) $
        Operation "gegl:text"
          [ Property "color" $ PropertyColor $ GEGL.RGBA 1 1 1 (1.1 - ttl)
          ]
      putAffection ud
        { fade = if (ttl - sec) > 0 then FadeIn (ttl - sec) else FadeOut 1
        , haskelloids = nhs
        }
    FadeOut ttl -> do
      liftIO $ gegl_node_set (nodeGraph ud M.! KeyMenuText) $
        Operation "gegl:text"
          [ Property "color" $ PropertyColor $ GEGL.RGBA 1 1 1 ttl
          ]
      putAffection ud
        { fade = if (ttl - sec) > 0 then FadeOut (ttl - sec) else FadeIn 1
        , haskelloids = nhs
        }
