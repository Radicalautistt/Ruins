module Ruins.Audio where

import qualified Apecs
import qualified SDL.Mixer as Mixer
import Control.Monad (unless)
import qualified Ruins.Resources as Resources
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Components.World as World

playSound :: Misc.Name -> World.RSystem ()
playSound soundName = do
  World.SoundMuted muted <- Apecs.get Apecs.global
  unless muted do
    World.SoundVolume currentVolume <- Apecs.get Apecs.global
    sound <- Resources.getResource World.sounds soundName
    Mixer.setVolume currentVolume sound
    Mixer.play sound

playMusic :: Misc.Name -> Mixer.Times -> World.RSystem ()
playMusic musicName times = do
  World.SoundMuted muted <- Apecs.get Apecs.global
  unless muted do
    Mixer.playMusic times =<<
      Resources.getResource World.music musicName

setAudioVolume :: Mixer.Volume -> World.RSystem ()
setAudioVolume volume = do
  Mixer.setMusicVolume volume
  Apecs.global Apecs.$= World.SoundVolume volume

toggleAudio :: World.RSystem ()
toggleAudio =
  Apecs.modify Apecs.global \ (World.SoundMuted muted) ->
    World.SoundMuted (not muted)
