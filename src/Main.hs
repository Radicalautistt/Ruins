{-# Language FlexibleContexts #-}

module Main where

import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Data.Text.IO as Text
import Control.Monad (unless)
import Control.Monad.Managed (with, runManaged)
import Ruins.Resources (loadResources, loadRoom)
import Ruins.Step (step)
import Ruins.EventHandler (animateIndefinitely)
import Ruins.Draw (drawGame)
import Ruins.Extra.SDL (initSDL, quitSDL, mkRectangle)
import Ruins.Extra.Apecs (newEntity_, mkPosition, unitVelocity)
import Ruins.Miscellaneous (mkName)
import Ruins.Components.Sprites (Sprite (..))
import Ruins.Components.World (RSystem, Time (..), Lever (..), QuitGame (..), Boundary (..),
                               Pressed (..), initRuins)

import Ruins.Components.Characters (Frisk (..), InFight (..), Napstablook (..),
                                    Action (..), Speed (..))

initGame :: RSystem ()
initGame = do
  -- | Debug level boundary.
  Apecs.set Apecs.global (MkBoundary 1305 0 510 116)

  newEntity_ (Frisk, MoveDown, MkSpeed 300, MkInFight False
           , APhysics.KinematicBody, mkPosition 0 300, unitVelocity)

  newEntity_ (Napstablook, MkInFight False, APhysics.StaticBody, mkPosition 300 200)

  newEntity_ (Lever, MkPressed False, APhysics.StaticBody
           , mkPosition 40 105, MkSprite (mkName "froggit", mkRectangle (0, 0) (19, 11)))

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
          Apecs.set Apecs.global window
          Apecs.set Apecs.global renderer
          loadResources
          loadRoom "debug.json"
          animateIndefinitely [mkName "froggit", mkName "napstablook"]
          gameLoop
