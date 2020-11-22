{-# Language MultiWayIf #-}
{-# Language RankNTypes #-}
{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}

module Ruins.EventHandler (
       handleEvents
     , handleKeyboardState
     ) where

import qualified SDL
import SDL.Input.Keyboard.Codes
import qualified Apecs
import qualified Linear
import Ruins.Apecs (unitVelocity, velocityVector, pattern VEL)
import Ruins.SDL (makeKeyPressed)
import Ruins.Components (RSystem, Frisk (..), Speed (..), Action (..), QuitGame (..))
import Control.Lens (Lens', (&), (+~), (-~))
import Control.Monad (when)
import qualified Language.Haskell.TH as THaskell

concat <$> traverse makeKeyPressed [
  THaskell.mkName "KeycodeEscape"
  ]

escapePressed :: SDL.EventPayload -> Bool
escapePressed = \ case
  PRESSED_ESCAPE -> True
  _otherwise -> False

increaseVelocityOf :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> RSystem ()
increaseVelocityOf entity axisLens = Apecs.modify entity \ (VEL _ _, MkSpeed speed) ->
  unitVelocity & velocityVector . axisLens +~ speed

-- | The name is probably bad since it kind of implies that I am making an entity slower
-- , while in fact I am moving it in a negative direction. I'll think about a better one.
decreaseVelocityOf :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> RSystem ()
decreaseVelocityOf entity axisLens = Apecs.modify entity \ (VEL _ _, MkSpeed speed) ->
  unitVelocity & velocityVector . axisLens -~ speed

dropVelocityOf :: Apecs.Entity -> RSystem ()
dropVelocityOf entity = Apecs.modify entity \ (VEL _ _) -> unitVelocity

-- | I need to handle keyboardState instead of SDL.Event
-- | for Frisk's movement because events aren't really responsive
-- | in cases of multiple key presses simulteneously.
-- | I've tried them but the latency was too real.
-- | So, I've googled up this approach and were happy with it ever since.
handleKeyboardState :: RSystem ()
handleKeyboardState = do
  keyIs <- SDL.getKeyboardState
  Apecs.cmapM_ \ (Frisk, friskEntity) ->
    if | keyIs SDL.ScancodeUp -> do
           Apecs.set friskEntity MoveUp
           decreaseVelocityOf friskEntity Linear._y

       | keyIs SDL.ScancodeDown -> do
           Apecs.set friskEntity MoveDown
           increaseVelocityOf friskEntity Linear._y

       | keyIs SDL.ScancodeLeft -> do
           Apecs.set friskEntity MoveLeft
           decreaseVelocityOf friskEntity Linear._x

       | keyIs SDL.ScancodeRight -> do
           Apecs.set friskEntity MoveRight
           increaseVelocityOf friskEntity Linear._y

       | otherwise -> dropVelocityOf friskEntity

handleEvents :: RSystem ()
handleEvents = do
  eventPayloads <- fmap SDL.eventPayload <$> SDL.pollEvents
  let quitEvent = SDL.QuitEvent `elem` eventPayloads
               || escapePressed `any` eventPayloads
  when quitEvent do
    Apecs.set Apecs.global (MkQuitGame True)
