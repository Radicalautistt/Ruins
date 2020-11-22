module Ruins.Step (step) where

import qualified Apecs
import qualified Apecs.Physics as APhysics
import Ruins.Components (RSystem, Time (..))
import Ruins.EventHandler (handleEvents, handleKeyboardState)

incrementTime :: Time -> RSystem ()
incrementTime deltaTime = Apecs.modify Apecs.global \ currentTime ->
  currentTime + deltaTime

step :: Time -> RSystem ()
step deltaTime@(MkTime dT) = do
  incrementTime deltaTime
  handleEvents
  handleKeyboardState
  APhysics.stepPhysics dT
