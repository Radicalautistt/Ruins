{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}

module Ruins.Keys where

import qualified SDL
import SDL.Input.Keyboard.Codes
import Data.Word (Word8)
import qualified Ruins.Extra.SDL as ESDL
import qualified Language.Haskell.TH as TH

-- | Generate pressed key patterns.
concat <$> traverse ESDL.makeKeyPressed [
  TH.mkName "KeycodeZ"
  , TH.mkName "KeycodeEscape"
  , TH.mkName "KeycodeC"
  , TH.mkName "KeycodeS"
  , TH.mkName "KeycodeM"
  , TH.mkName "KeycodeH"
  ]

pattern JOYSTICK_PRESSED :: Word8 -> SDL.EventPayload
pattern JOYSTICK_PRESSED button <-
  SDL.JoyButtonEvent
    (SDL.JoyButtonEventData _ button SDL.JoyButtonPressed)

pattern JOYSTICK_HAT_STATE :: SDL.JoyHatPosition -> SDL.EventPayload
pattern JOYSTICK_HAT_STATE hatPosition <-
  SDL.JoyHatEvent
    (SDL.JoyHatEventData _ _ hatPosition)

pattern JOYSTICK_A :: SDL.EventPayload
pattern JOYSTICK_A <- JOYSTICK_PRESSED 0

pattern JOYSTICK_B :: SDL.EventPayload
pattern JOYSTICK_B <- JOYSTICK_PRESSED 1

pattern JOYSTICK_Y :: SDL.EventPayload
pattern JOYSTICK_Y <- JOYSTICK_PRESSED 2

pattern JOYSTICK_X :: SDL.EventPayload
pattern JOYSTICK_X <- JOYSTICK_PRESSED 3

pattern JOYSTICK_UP :: SDL.EventPayload
pattern JOYSTICK_UP <- JOYSTICK_HAT_STATE SDL.HatUp

pattern JOYSTICK_DOWN :: SDL.EventPayload
pattern JOYSTICK_DOWN <- JOYSTICK_HAT_STATE SDL.HatDown

pattern JOYSTICK_LEFT :: SDL.EventPayload
pattern JOYSTICK_LEFT <- JOYSTICK_HAT_STATE SDL.HatLeft

pattern JOYSTICK_RIGHT :: SDL.EventPayload
pattern JOYSTICK_RIGHT <- JOYSTICK_HAT_STATE SDL.HatRight

pattern JOYSTICK_CENTERED :: SDL.EventPayload
pattern JOYSTICK_CENTERED <- JOYSTICK_HAT_STATE SDL.HatCentered

pattern JOYSTICK_BACK :: SDL.EventPayload
pattern JOYSTICK_BACK <- JOYSTICK_PRESSED 6

pattern JOYSTICK_START :: SDL.EventPayload
pattern JOYSTICK_START <- JOYSTICK_PRESSED 7

pattern JOYSTICK_BUMPER_LEFT :: SDL.EventPayload
pattern JOYSTICK_BUMPER_LEFT <- JOYSTICK_PRESSED 4

pattern JOYSTICK_BUMPER_RIGHT :: SDL.EventPayload
pattern JOYSTICK_BUMPER_RIGHT <- JOYSTICK_PRESSED 5
