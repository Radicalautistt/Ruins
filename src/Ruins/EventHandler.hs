{-# Language MultiWayIf #-}
{-# Language RankNTypes #-}
{-# Language ViewPatterns #-}

module Ruins.EventHandler (
       handleEvents
     , handleKeyboardState
     , animateIndefinitely
     ) where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Apecs
import qualified Apecs.Physics as Physics
import qualified Linear
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HMap
import Control.Lens (Lens', set, over, (&), (+~), (-~))
import Control.Monad (when)
import qualified Ruins.Keys as Keys
import qualified Ruins.Audio as Audio
import qualified Ruins.Script as Script
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.Spawn as Spawn
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

givePositiveVelocity :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> World.RSystem ()
givePositiveVelocity entity axisLens =
  Apecs.modify entity \ (EApecs.VEL _ _, Characters.Speed speed) ->
    EApecs.unitVelocity & EApecs.velocityVector . axisLens +~ speed

giveNegativeVelocity :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> World.RSystem ()
giveNegativeVelocity entity axisLens =
  Apecs.modify entity \ (EApecs.VEL _ _, Characters.Speed speed) ->
    EApecs.unitVelocity & EApecs.velocityVector . axisLens -~ speed

dropVelocityOf :: Apecs.Entity -> World.RSystem ()
dropVelocityOf entity = Apecs.modify entity \ (EApecs.VEL _ _) -> EApecs.unitVelocity

{-# Inline toggleAnimation #-}
toggleAnimation :: Misc.Name -> Bool -> World.RSystem ()
toggleAnimation spriteSheetName enabled =
  Apecs.modify Apecs.global $
    over World.sprites (HMap.update (Just . set Sprites.animated enabled) spriteSheetName)

animate :: Misc.Name -> World.RSystem ()
animate spriteSheetName = toggleAnimation spriteSheetName True

stopAnimation :: Misc.Name -> World.RSystem ()
stopAnimation spriteSheetName = toggleAnimation spriteSheetName False

-- | Enable animations by default for the given spritesheet names.
-- | Useful when you have some NPCs with a looping animation routine.
animateIndefinitely :: [Misc.Name] -> World.RSystem ()
animateIndefinitely names = for_ names animate

move :: Apecs.Entity -> Characters.Action -> Misc.Name -> World.RSystem ()
move entity direction animationName = do
  Apecs.set entity direction
  setVelocityValue
  animate animationName
  where setVelocityValue =
          case direction of
            Characters.MoveUp ->
              giveNegativeVelocity entity Linear._y

            Characters.MoveDown ->
              givePositiveVelocity entity Linear._y

            Characters.MoveLeft ->
              giveNegativeVelocity entity Linear._x

            Characters.MoveRight ->
              givePositiveVelocity entity Linear._x

stop :: Apecs.Entity -> Misc.Name -> World.RSystem ()
stop entity animationName = do
  dropVelocityOf entity
  stopAnimation animationName

handleKeyboardState :: World.RSystem ()
handleKeyboardState = do
  keyIs <- SDL.getKeyboardState
  Apecs.cmapM_ \ (Characters.Frisk, friskEntity) -> do
    let moveFrisk direction = move friskEntity direction "frisk"

    if | keyIs SDL.ScancodeUp -> moveFrisk Characters.MoveUp
       | keyIs SDL.ScancodeDown -> moveFrisk Characters.MoveDown
       | keyIs SDL.ScancodeLeft -> moveFrisk Characters.MoveLeft
       | keyIs SDL.ScancodeRight -> moveFrisk Characters.MoveRight
       | otherwise -> stop friskEntity "frisk"

handleEvent :: SDL.EventPayload -> World.RSystem ()
handleEvent = \ case
  Keys.PRESSED_C -> Apecs.modify Apecs.global (over World.cameraActive not)
  Keys.PRESSED_S -> Audio.playSound "hurt"
  Keys.PRESSED_M -> Audio.playMusic "megalovania-low" Mixer.Once
  Keys.PRESSED_H -> Audio.toggleAudio

  Keys.PRESSED_Z ->
    Apecs.cmapM_ \ (World.Lever, Physics.Position leverP, World.Pressed pressed, lever) ->
      Apecs.cmapM_ \ (Characters.Frisk, Physics.Position friskP) -> do
        Apecs.modify lever \ (World.Pressed p) ->
          if Linear.norm (leverP - friskP) < 30
             then World.Pressed (not p)
                  else World.Pressed p

        if not pressed
         then do Spawn.spawnFroggit (EApecs.mkPosition 300 400)
                 Script.say "I have no reason to live" 0.09 Nothing "default-voice"
              else do Apecs.cmap \ Characters.Froggit ->
                       Apecs.Not @Characters.Froggit
                      Apecs.modify Apecs.global (set World.opened False)

  Keys.PRESSED_ESCAPE -> Apecs.set Apecs.global (World.QuitGame True)

  Keys.JOYSTICK_UP -> moveFrisk Characters.MoveUp
  Keys.JOYSTICK_DOWN -> moveFrisk Characters.MoveDown
  Keys.JOYSTICK_LEFT -> moveFrisk Characters.MoveLeft
  Keys.JOYSTICK_RIGHT -> moveFrisk Characters.MoveRight
  Keys.JOYSTICK_CENTERED -> Apecs.cmapM \ (Characters.Frisk, friskEntity) ->
    stop friskEntity "frisk"

  _otherwise -> pure ()
  where moveFrisk direction = Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
          move friskEntity direction "frisk"

handleEvents :: World.RSystem ()
handleEvents = do
  (map SDL.eventPayload -> eventPayloads) <- SDL.pollEvents
  when (SDL.QuitEvent `elem` eventPayloads) do
    Apecs.set Apecs.global (World.QuitGame True)

  for_ eventPayloads handleEvent
