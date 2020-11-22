{-# Language FlexibleContexts #-}

module Main where

import qualified SDL
import qualified Apecs
import Ruins.SDL (initSDL, quitSDL)
import Control.Monad (unless)
import Control.Monad.Managed (with, runManaged)
import Ruins.Resources (loadResources, getResource)
import Ruins.Step (step)
import Ruins.Components (RSystem, Time (..), Window (..), Renderer (..), SpriteSheet (..),
                         QuitGame (..), pattern Renderer, initRuins, sprites, mkName)

gameLoop :: RSystem ()
gameLoop = do
  Renderer renderer <- Apecs.get Apecs.global
  MkQuitGame quitGame <- Apecs.get Apecs.global
  MkSpriteSheet {..} <- getResource sprites (mkName "frisk")
  step (MkTime (1 / 60))
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255
  SDL.copy renderer _spriteSheet Nothing Nothing
  SDL.present renderer
  unless quitGame do gameLoop

main :: IO ()
main = do
  world <- initRuins
  with initSDL do runManaged . Apecs.runWith world . gameRoutine
  quitSDL
  where gameRoutine (window, renderer) = do
          Apecs.set Apecs.global (MkWindow (Just window))
          Apecs.set Apecs.global (MkRenderer (Just renderer))
          loadResources
          gameLoop
