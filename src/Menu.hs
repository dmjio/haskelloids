module Menu where

import Affection
import qualified SDL

import Debug.Trace

import Types

handleMenuEvent :: Double -> SDL.Event -> Affection UserData ()
handleMenuEvent sec e = do
  ad <- get
  case SDL.eventPayload e of
    SDL.WindowClosedEvent _ -> do
      traceM "seeya!"
      put ad
        { quitEvent = True
        }
    _ -> return ()
