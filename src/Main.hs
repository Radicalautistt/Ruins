module Main where

import Ruins.SDL
import Control.Monad.Managed (with)

main :: IO ()
main = do
  with initSDL do gameRoutine
  quitSDL
  where gameRoutine _ = pure ()
