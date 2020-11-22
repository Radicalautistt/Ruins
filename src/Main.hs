{-# Language FlexibleContexts #-}

module Main where

import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Data.Text.IO as Text
import Ruins.SDL (initSDL, quitSDL)
import Control.Monad (unless)
import Control.Monad.Managed (with, runManaged)
import Ruins.Resources (loadResources)
import Ruins.Step (step)
import Ruins.Draw (drawGame)
import Ruins.Apecs (newEntity_, unitPosition, unitVelocity)
import Ruins.Components (RSystem, Time (..), Frisk (..), Window (..), Renderer (..),
                         QuitGame (..), Action (..), Speed (..), initRuins)

initGame :: RSystem ()
initGame = do
  newEntity_ (Frisk, MoveDown, MkSpeed 300
           , APhysics.KinematicBody, unitPosition, unitVelocity)

gameLoop :: RSystem ()
gameLoop = do
  MkQuitGame quitGame <- Apecs.get Apecs.global
  step (MkTime (1 / 60))
  drawGame
  unless quitGame do gameLoop

main :: IO ()
main = do
  world <- initRuins
  with initSDL do runManaged . Apecs.runWith world . gameRoutine
  Text.putStrLn "Farewell"
  quitSDL
  where gameRoutine (window, renderer) = do
          initGame
          Apecs.set Apecs.global (MkWindow (Just window))
          Apecs.set Apecs.global (MkRenderer (Just renderer))
          loadResources
          gameLoop
