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
import qualified SDL.Mixer as Mixer
import SDL.Input.Keyboard.Codes
import qualified Apecs
import qualified Apecs.Physics as Physics
import qualified Linear
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HMap
import Control.Lens (Lens', set, over, (&), (+~), (-~))
import Control.Monad (when)
import qualified Language.Haskell.TH as TH
import qualified Ruins.Audio as Audio
import qualified Ruins.Script as Script
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Extra.SDL as ESDL
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.Spawn as Spawn
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

-- | Generate pressed key patterns.
concat <$> traverse ESDL.makeKeyPressed [
  TH.mkName "KeycodeZ"
  , TH.mkName "KeycodeEscape"
  , TH.mkName "KeycodeC"
  , TH.mkName "KeycodeS"
  , TH.mkName "KeycodeM"
  , TH.mkName "KeycodeH"
  ]

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

-- | I need to handle keyboardState instead of SDL.Event
-- | for Frisk's movement because events aren't really responsive
-- | in cases of multiple key presses simultaneously.
-- | I've tried them but the latency was too real.
-- | So, I've googled up this approach and were happy with it ever since.
handleKeyboardState :: World.RSystem ()
handleKeyboardState = do
  keyIs <- SDL.getKeyboardState
  Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
    if | keyIs SDL.ScancodeUp -> do
           Apecs.set friskEntity Characters.MoveUp
           giveNegativeVelocity friskEntity Linear._y
           animate "frisk"

       | keyIs SDL.ScancodeDown -> do
           Apecs.set friskEntity Characters.MoveDown
           givePositiveVelocity friskEntity Linear._y
           animate "frisk"

       | keyIs SDL.ScancodeLeft -> do
           Apecs.set friskEntity Characters.MoveLeft
           giveNegativeVelocity friskEntity Linear._x
           animate "frisk"

       | keyIs SDL.ScancodeRight -> do
           Apecs.set friskEntity Characters.MoveRight
           givePositiveVelocity friskEntity Linear._x
           animate "frisk"

       | otherwise -> do
           dropVelocityOf friskEntity
           stopAnimation "frisk"

handleEvent :: SDL.EventPayload -> World.RSystem ()
handleEvent = \ case
  PRESSED_C -> Apecs.modify Apecs.global (over World.cameraActive not)
  PRESSED_S -> Audio.playSound "hurt"
  PRESSED_M -> Audio.playMusic "megalovania-low" Mixer.Once
  PRESSED_H -> Audio.toggleAudio

  PRESSED_Z ->
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

  PRESSED_ESCAPE -> Apecs.set Apecs.global (World.QuitGame True)

  _otherwise -> pure ()

handleEvents :: World.RSystem ()
handleEvents = do
  (map SDL.eventPayload -> eventPayloads) <- SDL.pollEvents
  when (SDL.QuitEvent `elem` eventPayloads) do
    Apecs.set Apecs.global (World.QuitGame True)

  for_ eventPayloads handleEvent
