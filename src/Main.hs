{-# Language FlexibleContexts #-}

module Main where

import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Data.Text.IO as Text
import Ruins.SDL (initSDL, quitSDL)
import Control.Monad (unless)
import Control.Monad.Managed (with, runManaged)
import qualified Data.ByteString as BString
import qualified Data.ByteString.Char8 as BString
import Ruins.Resources (loadResources)
import Ruins.Step (step)
import Ruins.Draw (drawGame)
import Ruins.Apecs (newEntity_, mkPosition, unitVelocity)
import Ruins.Components (RSystem, Time (..), Frisk (..), Window (..), Renderer (..),
                         QuitGame (..), Action (..), Speed (..), Boundary (..), initRuins)

generateDebugRoom :: IO ()
generateDebugRoom =
  BString.writeFile "assets/rooms/debug-room.json" roomConfig
  where wallTile :: BString.ByteString
        wallTile = "{ \"tile-solid\":true, \"tile-rectangle\": [21, 803, 20, 20] }"
        floorTile :: BString.ByteString
        floorTile = "{ \"tile-solid\":false, \"tile-rectangle\": [203, 643, 20, 20] }"

        walls = "[ " <> BString.intercalate ", " (replicate 25 wallTile) <> " ]"
        floor' = "[ " <> BString.intercalate ", " (replicate 25 floorTile) <> " ]"

        roomConfig = BString.unlines [
          "{"
          , "\"tile-map\": ["
          , BString.intercalate ",\n" (replicate 5 walls) <> ","
          , BString.intercalate ",\n" (replicate 10 floor') <> ","
          , BString.intercalate ",\n" (replicate 5 walls)
          , "],"
          , "\"source-name\": \"ruins-tiles\""
          , "}"
          ]

initGame :: RSystem ()
initGame = do
  -- | Debug level boundaries.
  Apecs.set Apecs.global (MkBoundary 1305 0 510 116)
  newEntity_ (Frisk, MoveDown, MkSpeed 300
           , APhysics.KinematicBody, mkPosition 0 300, unitVelocity)

gameLoop :: RSystem ()
gameLoop = do
  MkQuitGame quitGame <- Apecs.get Apecs.global
  step (MkTime (1 / 60))
  drawGame
  unless quitGame do gameLoop

main :: IO ()
main = do
  generateDebugRoom
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
