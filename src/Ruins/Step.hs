module Ruins.Step (step) where

import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Data.Vector as Vector
import GHC.Int (neInt)
import Data.Bool (bool)
import Control.Lens (over, each, (&), (.~), (+~))
import Ruins.EventHandler (handleEvents, handleKeyboardState)
import Ruins.Components (RSystem, Time (..), Animation (..),
                         currentClipIndex, sprites, animations)

incrementTime :: Time -> RSystem ()
incrementTime deltaTime = Apecs.modify Apecs.global \ currentTime ->
  currentTime + deltaTime

stepAnimations :: Time -> RSystem ()
stepAnimations (MkTime deltaTime) = do
  MkTime currentTime <- Apecs.get Apecs.global
  let stepAnimation animation@MkAnimation {..} =
        let stepNeeded =
              floor (currentTime / _delay) `neInt` floor ((currentTime + deltaTime) / _delay)
        in case _clips Vector.!? _currentClipIndex of
          Nothing -> animation & currentClipIndex .~ 0
          Just _rect ->
            bool animation (animation & currentClipIndex +~ 1) (_active && stepNeeded)

  Apecs.modify Apecs.global
    (over (sprites . each . animations . each) stepAnimation)

step :: Time -> RSystem ()
step deltaTime@(MkTime dT) = do
  incrementTime deltaTime
  handleEvents
  handleKeyboardState
  stepAnimations deltaTime
  APhysics.stepPhysics dT
