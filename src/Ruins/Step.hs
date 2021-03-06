{-# Language CPP #-}
{-# Language NamedFieldPuns #-}

module Ruins.Step (step) where

import qualified SDL.Mixer as Mixer
import qualified Apecs
import qualified Apecs.Physics as Physics
import qualified Data.Vector as Vector
import qualified Linear
import GHC.Int (neInt)
import Data.Bool (bool)
import qualified Data.Text as Text
import Data.Foldable (for_)
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
clamp :: Physics.Position -> (Double, Double, Double, Double) -> Physics.Position
clamp (EApecs.XY x y) (xmax, xmin, ymax, ymin) =
  EApecs.mkPosition (min xmax (max xmin x)) (min ymax (max ymin y))

-- clampFrisk :: World.RSystem ()
-- clampFrisk = do
--   Sprites.Background {_backgroundBoundary} <- Apecs.get Apecs.global
--   Apecs.cmap \ (Characters.Frisk, friskPosition) ->
--     clamp friskPosition _backgroundBoundary

clampCamera :: World.RSystem ()
clampCamera = do
  Sprites.Background {_backgroundBoundary} <- Apecs.get Apecs.global
  Apecs.modify Apecs.global \ camera@World.Camera{_cameraActive} ->
    if not _cameraActive
       then camera
            else camera & World.cameraOffset %~ \ offset ->
              clamp (Physics.Position offset) _backgroundBoundary ^. EApecs.positionVector

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
    case _textBoxOpened && stepNeeded currentTime deltaTime _textBoxLetterDelay of
      shouldStep
        | Text.take _textBoxVisibleChunk _textBoxCurrentText == _textBoxCurrentText ->
            textBox & World.textBoxActive .~ False
        | otherwise -> bool textBox (textBox & World.textBoxVisibleChunk +~ 1) shouldStep

voiceTextBox :: World.Time -> World.RSystem ()
voiceTextBox deltaTime = do
  currentTime <- Apecs.get Apecs.global
  World.TextBox {..} <- Apecs.get Apecs.global
  voice <- Resources.getResource World.sounds _textBoxVoiceSound
  when (_textBoxOpened && stepNeeded currentTime deltaTime _textBoxLetterDelay
    && (Text.last (Text.take _textBoxVisibleChunk _textBoxCurrentText) /= ' ')) do
    Mixer.setChannels (Text.length _textBoxCurrentText)
    Mixer.setVolume 30 voice
    void (liftIO (Mixer.playOn (fromIntegral _textBoxVisibleChunk) Mixer.Once voice))

stepCamera :: World.RSystem ()
stepCamera = Apecs.cmapM_ \ (Characters.Frisk, Physics.Position friskPosition) ->
  Apecs.modify Apecs.global \ camera@World.Camera {_cameraActive} ->
    if not _cameraActive
       then camera
            else camera &~ do
                   World.cameraOffset .= Linear.zero
                   World.cameraOffset += (friskPosition - 200)

stepRoom :: World.RSystem ()
stepRoom = Apecs.cmapM_ \ (Characters.Frisk, Physics.Position friskPosition) -> do
  World.Divarication paths <- Apecs.get Apecs.global
  for_ paths \ (Physics.Position roomPosition, roomFilePath) ->
    when (Linear.norm (roomPosition - friskPosition) < 30) do
      Resources.loadRoom roomFilePath

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
  Physics.stepPhysics dT
  stepCamera
  -- clampFrisk
  stepRoom
  clampCamera
