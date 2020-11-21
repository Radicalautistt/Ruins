{-# Language FlexibleContexts #-}

module Main where

import qualified SDL
import qualified Apecs
import Ruins.SDL
import Control.Monad (unless)
import Control.Monad.Managed (with, runManaged)
import Ruins.Components (RSystem, Window (..), Renderer (..), SpriteSheet (..), pattern Renderer, initRuins, sprites, mkName)
import Ruins.Resources (loadResources, getResource)

gameLoop :: RSystem ()
gameLoop = do
  Renderer renderer <- Apecs.get Apecs.global
  eventPayloads <- fmap SDL.eventPayload <$> SDL.pollEvents
  let quitGame = SDL.QuitEvent `elem` eventPayloads
  MkSpriteSheet {..} <- getResource sprites (mkName "frisk")
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255
  SDL.copy renderer _spriteSheet Nothing Nothing
  SDL.present renderer
  unless quitGame do gameLoop


main :: IO ()
main = do
  world <- initRuins
  with initSDL do (runManaged . Apecs.runWith world . gameRoutine)
  quitSDL
  where gameRoutine (window, renderer) = do
          Apecs.set Apecs.global (MkWindow (Just window))
          Apecs.set Apecs.global (MkRenderer (Just renderer))
          loadResources
          gameLoop
