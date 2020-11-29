module Ruins.Step (step) where

import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Data.Vector as Vector
import GHC.Int (neInt)
import Data.Bool (bool)
import qualified Data.Text as Text
import Control.Lens (over, each, (&), (.~), (+~), (%~))
import Ruins.Apecs (pattern XY, mkPosition)
import Ruins.EventHandler (handleEvents, handleKeyboardState)
import Ruins.Components (RSystem, Time (..), Animation (..), Frisk (..), Boundary (..),
                         SpriteSheet (..), TextBox (..), visibleChunk, currentClipIndex, sprites, animations)

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

stepAnimations :: Time -> RSystem ()
stepAnimations (MkTime deltaTime) = do
  MkTime currentTime <- Apecs.get Apecs.global
  let stepAnimation spriteSheet@MkSpriteSheet {..} =
        spriteSheet & animations . each %~ \ animation@MkAnimation {..} ->
          let stepNeeded =
                floor (currentTime / _delay) `neInt` floor ((currentTime + deltaTime) / _delay)
          in case _clips Vector.!? _currentClipIndex of
            Nothing -> animation & currentClipIndex .~ 0
            Just _rect ->
              bool animation (animation & currentClipIndex +~ 1) (_animated && stepNeeded)

  Apecs.modify Apecs.global
    (over (sprites . each) stepAnimation)

-- | TODO generalize stepTextBox and stepAnimations.
stepTextBox :: Time -> RSystem ()
stepTextBox (MkTime deltaTime) = do
  MkTime currentTime <- Apecs.get Apecs.global
  Apecs.modify Apecs.global \ textBox@MkTextBox {..} ->
    let stepNeeded =
          floor (currentTime / _letterDelay) `neInt` floor ((currentTime + deltaTime) / _letterDelay)
    in if Text.take _visibleChunk _currentText == _currentText
          then textBox & visibleChunk .~ 1
               else bool textBox (textBox & visibleChunk +~ 1) (_opened && stepNeeded)

step :: Time -> RSystem ()
step deltaTime@(MkTime dT) = do
  incrementTime deltaTime
  handleEvents
  handleKeyboardState
  stepTextBox deltaTime
  stepAnimations deltaTime
  APhysics.stepPhysics dT
  clampFrisk
