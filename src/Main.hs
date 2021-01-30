{-# Language FlexibleContexts #-}

module Main where

import qualified SDL
import qualified Apecs
import qualified Data.Text.IO as Text
import Control.Monad (unless)
import Control.Monad.Managed (with, runManaged)
import qualified Ruins.Step as Step
import qualified Ruins.Draw as Draw
import qualified Ruins.Audio as Audio
import qualified Ruins.Resources as Resources
import qualified Ruins.EventHandler as EventHandler
import qualified Ruins.Extra.SDL as ESDL
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.Spawn as Spawn
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Characters as Characters

initGame :: SDL.Window -> SDL.Renderer -> World.RSystem ()
initGame window renderer = do
  Audio.setAudioVolume 30
  Apecs.global Apecs.$= window
  Apecs.global Apecs.$= renderer

  Spawn.spawnFrisk (EApecs.mkPosition 0 400) False Characters.MoveDown
  Spawn.spawnLever (EApecs.mkPosition 40 300)

gameLoop :: World.RSystem ()
gameLoop = do
  World.QuitGame quitGame <- Apecs.get Apecs.global
  Step.step (World.Time (1 / 60))
  Draw.drawGame
  unless quitGame do gameLoop

main :: IO ()
main = do
  world <- World.initRuins
  with ESDL.initSDL do runManaged . Apecs.runWith world . gameRoutine
  Text.putStrLn "Farewell"
  ESDL.quitSDL
  where gameRoutine gameData = do
          initGame `uncurry` gameData
          Resources.loadResources
          Resources.loadRoom "debug.json"
          EventHandler.animateIndefinitely ["froggit", "napstablook"]
          gameLoop
