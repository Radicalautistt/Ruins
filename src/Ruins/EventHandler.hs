{-# Language MultiWayIf #-}
{-# Language RankNTypes #-}
{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}

module Ruins.EventHandler (
       handleEvents
     , handleKeyboardState
     , animateIndefinitely
     ) where

import qualified SDL
import SDL.Input.Keyboard.Codes
import qualified Apecs
import qualified Apecs.Physics as APhysics
import qualified Linear
import Ruins.Apecs (unitVelocity, velocityVector, mkPosition, newEntity_, pattern VEL)
import Ruins.SDL (makeKeyPressed)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HMap
import Control.Lens (Lens', set, over, (&), (+~), (-~))
import Control.Monad (when)
import qualified Language.Haskell.TH as THaskell
import Ruins.Components (RSystem, Frisk (..), Speed (..), Action (..), mkName, Name,
                         QuitGame (..), Froggit (..), Lever (..), Pressed (..), sprites, animated)

-- | Generate pressed key patterns.
concat <$> traverse makeKeyPressed [
  THaskell.mkName "KeycodeZ"
  , THaskell.mkName "KeycodeEscape"
  ]

escapePressed :: SDL.EventPayload -> Bool
escapePressed = \ case
  PRESSED_ESCAPE -> True
  _otherwise -> False

givePositiveVelocity :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> RSystem ()
givePositiveVelocity entity axisLens = Apecs.modify entity \ (VEL _ _, MkSpeed speed) ->
  unitVelocity & velocityVector . axisLens +~ speed

giveNegativeVelocity :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> RSystem ()
giveNegativeVelocity entity axisLens = Apecs.modify entity \ (VEL _ _, MkSpeed speed) ->
  unitVelocity & velocityVector . axisLens -~ speed

dropVelocityOf :: Apecs.Entity -> RSystem ()
dropVelocityOf entity = Apecs.modify entity \ (VEL _ _) -> unitVelocity

{-# Inline toggleAnimation #-}
toggleAnimation :: Name -> Bool ->  RSystem ()
toggleAnimation spriteSheetName enabled =
  Apecs.modify Apecs.global $
    over sprites (HMap.update (Just . set animated enabled) spriteSheetName)

animate :: Name -> RSystem ()
animate spriteSheetName = toggleAnimation spriteSheetName True

stopAnimation :: Name -> RSystem ()
stopAnimation spriteSheetName = toggleAnimation spriteSheetName False

-- | Enable animations by default for the given spritesheet names.
-- | Useful when you have some NPCs with a looping animation routine.
animateIndefinitely :: [Name] -> RSystem ()
animateIndefinitely names = for_ names animate

-- | I need to handle keyboardState instead of SDL.Event
-- | for Frisk's movement because events aren't really responsive
-- | in cases of multiple key presses simultaneously.
-- | I've tried them but the latency was too real.
-- | So, I've googled up this approach and were happy with it ever since.
handleKeyboardState :: RSystem ()
handleKeyboardState = do
  keyIs <- SDL.getKeyboardState
  Apecs.cmapM_ \ (Frisk, friskEntity) ->
    if | keyIs SDL.ScancodeUp -> do
           Apecs.set friskEntity MoveUp
           giveNegativeVelocity friskEntity Linear._y
           animate (mkName "frisk")

       | keyIs SDL.ScancodeDown -> do
           Apecs.set friskEntity MoveDown
           givePositiveVelocity friskEntity Linear._y
           animate (mkName "frisk")

       | keyIs SDL.ScancodeLeft -> do
           Apecs.set friskEntity MoveLeft
           giveNegativeVelocity friskEntity Linear._x
           animate (mkName "frisk")

       | keyIs SDL.ScancodeRight -> do
           Apecs.set friskEntity MoveRight
           givePositiveVelocity friskEntity Linear._x
           animate (mkName "frisk")

       | otherwise -> do
           dropVelocityOf friskEntity
           stopAnimation (mkName "frisk")

handleEvent :: SDL.EventPayload -> RSystem ()
handleEvent = \ case
  PRESSED_Z ->
    Apecs.cmapM_ \ (Lever, APhysics.Position leverP, MkPressed p, lever) ->
      Apecs.cmapM_ \ (Frisk, APhysics.Position friskP) -> do
        Apecs.modify lever \ (MkPressed pressed) ->
          if Linear.norm (leverP - friskP) < 30
             then MkPressed (not pressed)
                  else MkPressed pressed

        if not p
           then newEntity_ (Froggit, APhysics.StaticBody, mkPosition 300 300)
                else Apecs.cmap \ Froggit ->
                       Apecs.Not @Froggit

  _otherwise -> pure ()

handleEvents :: RSystem ()
handleEvents = do
  eventPayloads <- fmap SDL.eventPayload <$> SDL.pollEvents
  let quitEvent = SDL.QuitEvent `elem` eventPayloads
               || escapePressed `any` eventPayloads
  when quitEvent do
    Apecs.set Apecs.global (MkQuitGame True)

  for_ eventPayloads handleEvent
