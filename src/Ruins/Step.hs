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
import Ruins.Extra.Apecs (pattern XY, mkPosition, positionVector)
import Control.Monad (when, void)
import Unsafe.Coerce (unsafeCoerce)
import Ruins.Resources (getResource)
import Ruins.EventHandler (handleEvents, handleKeyboardState)
import Ruins.Components.Sprites (Animation (..), SpriteSheet (..), animations, currentClipIndex)
import Ruins.Components.Characters (Frisk (..))
import Ruins.Components.World (RSystem, Time (..), Boundary (..), TextBox (..), Camera (..),
                               cameraOffset, visibleChunk, sprites, sounds)

incrementTime :: Time -> RSystem ()
incrementTime deltaTime = Apecs.modify Apecs.global \ currentTime ->
  currentTime + deltaTime

{-# Inline clamp #-}
clamp :: APhysics.Position -> Boundary -> APhysics.Position
clamp (XY x y) MkBoundary {..} =
  mkPosition (min xmax (max xmin x)) (min ymax (max ymin y))

clampFrisk :: RSystem ()
clampFrisk = do
  boundary <- Apecs.get Apecs.global
  Apecs.cmap \ (Frisk, friskPosition) ->
    clamp friskPosition boundary

clampCamera :: RSystem ()
clampCamera = do
  boundary <- Apecs.get Apecs.global
  Apecs.modify Apecs.global \ camera@MkCamera{_cameraActive} ->
    if not _cameraActive
       then camera
            else camera & cameraOffset %~ \ offset ->
              clamp (APhysics.Position offset) boundary ^. positionVector

{-# Inline stepNeeded #-}
stepNeeded :: Time -> Time -> Double -> Bool
stepNeeded (MkTime currentTime) (MkTime deltaTime) delay =
  floor (currentTime / delay) `neInt` floor ((currentTime + deltaTime) / delay)

stepAnimations :: Time -> RSystem ()
stepAnimations deltaTime = do
  currentTime <- Apecs.get Apecs.global
  let stepAnimation spriteSheet@MkSpriteSheet {..} =
        spriteSheet & animations . each %~ \ animation@MkAnimation {..} ->
          case _animationClips Vector.!? _currentClipIndex of
            Nothing -> animation & currentClipIndex .~ 0
            Just _rect ->
              bool animation (animation & currentClipIndex +~ 1)
                (_animated && stepNeeded currentTime deltaTime _animationDelay)

  Apecs.modify Apecs.global
    (over (sprites . each) stepAnimation)

stepTextBox :: Time -> RSystem ()
stepTextBox deltaTime = do
  currentTime <- Apecs.get Apecs.global
  Apecs.modify Apecs.global \ textBox@MkTextBox {..} ->
    if Text.take _visibleChunk _currentText == _currentText
       then textBox
            else bool textBox (textBox & visibleChunk +~ 1)
                   (_opened && stepNeeded currentTime deltaTime _letterDelay)

voiceTextBox :: Time -> RSystem ()
voiceTextBox deltaTime = do
  currentTime <- Apecs.get Apecs.global
  MkTextBox {..} <- Apecs.get Apecs.global
  voice <- getResource sounds _voiceSound
  when (_opened && stepNeeded currentTime deltaTime _letterDelay
    && (Text.last (Text.take _visibleChunk _currentText) /= ' ')) do
    Mixer.setChannels (Text.length _currentText)
    Mixer.setVolume 30 voice
    void (liftIO (Mixer.playOn (unsafeCoerce _visibleChunk) Mixer.Once voice))

stepCamera :: RSystem ()
stepCamera = Apecs.cmapM_ \ (Frisk, APhysics.Position friskPosition) ->
  Apecs.modify Apecs.global \ camera@MkCamera {_cameraActive} ->
    if not _cameraActive
       then camera
            else camera &~ do
                   cameraOffset .= Linear.zero
                   cameraOffset += friskPosition

step :: Time -> RSystem ()
step deltaTime@(MkTime dT) = do
  incrementTime deltaTime
  handleEvents
  handleKeyboardState
  stepTextBox deltaTime
  voiceTextBox deltaTime
  stepAnimations deltaTime
  APhysics.stepPhysics dT
  stepCamera
  -- clampFrisk
  clampCamera
