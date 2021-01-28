{-# Language CPP #-}
{-# Language NamedFieldPuns #-}

module Ruins.Step (step) where

import qualified SDL.Mixer as Mixer
import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Data.Vector as Vector
import qualified Linear
import GHC.Int (neInt)
import Data.Bool (bool)
import qualified Data.Text as Text
import Control.Lens (over, each, (&), (^.), (.~), (+~), (%~), (&~), (.=), (+=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import qualified Ruins.Resources as Resources
import qualified Ruins.EventHandler as EventHandler
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

-- #define __JOYSTICK

incrementTime :: World.Time -> World.RSystem ()
incrementTime deltaTime =
  Apecs.modify Apecs.global \ currentTime ->
    currentTime + deltaTime

{-# Inline clamp #-}
clamp :: APhysics.Position -> World.Boundary -> APhysics.Position
clamp (EApecs.XY x y) World.Boundary {..} =
  EApecs.mkPosition (min xmax (max xmin x)) (min ymax (max ymin y))

-- clampFrisk :: World.RSystem ()
-- clampFrisk = do
--   boundary <- Apecs.get Apecs.global
--   Apecs.cmap \ (Characters.Frisk, friskPosition) ->
--     clamp friskPosition boundary

clampCamera :: World.RSystem ()
clampCamera = do
  boundary <- Apecs.get Apecs.global
  Apecs.modify Apecs.global \ camera@World.Camera{_cameraActive} ->
    if not _cameraActive
       then camera
            else camera & World.cameraOffset %~ \ offset ->
              clamp (APhysics.Position offset) boundary ^. EApecs.positionVector

{-# Inline stepNeeded #-}
stepNeeded :: World.Time -> World.Time -> Double -> Bool
stepNeeded (World.Time currentTime) (World.Time deltaTime) delay =
  floor (currentTime / delay) `neInt` floor ((currentTime + deltaTime) / delay)

stepAnimations :: World.Time -> World.RSystem ()
stepAnimations deltaTime = do
  currentTime <- Apecs.get Apecs.global
  let stepAnimation spriteSheet@Sprites.SpriteSheet {..} =
        spriteSheet & Sprites.animations . each %~ \ animation@Sprites.Animation {..} ->
          case _animationClips Vector.!? _currentClipIndex of
            Nothing -> animation & Sprites.currentClipIndex .~ 0
            Just _rect ->
              bool animation (animation & Sprites.currentClipIndex +~ 1)
                (_animated && stepNeeded currentTime deltaTime _animationDelay)

  Apecs.modify Apecs.global
    (over (World.sprites . each) stepAnimation)

stepTextBox :: World.Time -> World.RSystem ()
stepTextBox deltaTime = do
  currentTime <- Apecs.get Apecs.global
  Apecs.modify Apecs.global \ textBox@World.TextBox {..} ->
    if Text.take _visibleChunk _currentText == _currentText
       then textBox
            else bool textBox (textBox & World.visibleChunk +~ 1)
                   (_opened && stepNeeded currentTime deltaTime _letterDelay)

voiceTextBox :: World.Time -> World.RSystem ()
voiceTextBox deltaTime = do
  currentTime <- Apecs.get Apecs.global
  World.TextBox {..} <- Apecs.get Apecs.global
  voice <- Resources.getResource World.sounds _voiceSound
  when (_opened && stepNeeded currentTime deltaTime _letterDelay
    && (Text.last (Text.take _visibleChunk _currentText) /= ' ')) do
    Mixer.setChannels (Text.length _currentText)
    Mixer.setVolume 30 voice
    void (liftIO (Mixer.playOn (fromIntegral _visibleChunk) Mixer.Once voice))

stepCamera :: World.RSystem ()
stepCamera = Apecs.cmapM_ \ (Characters.Frisk, APhysics.Position friskPosition) ->
  Apecs.modify Apecs.global \ camera@World.Camera {_cameraActive} ->
    if not _cameraActive
       then camera
            else camera &~ do
                   World.cameraOffset .= Linear.zero
                   World.cameraOffset += friskPosition

step :: World.Time -> World.RSystem ()
step deltaTime@(World.Time dT) = do
  incrementTime deltaTime
  EventHandler.handleEvents
#ifndef __JOYSTICK
  EventHandler.handleKeyboardState
#endif
  stepTextBox deltaTime
  voiceTextBox deltaTime
  stepAnimations deltaTime
  APhysics.stepPhysics dT
  stepCamera
  -- clampFrisk
  clampCamera
