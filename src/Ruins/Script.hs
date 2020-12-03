module Ruins.Script (say) where

import qualified Apecs
import Data.Text (Text)
import Control.Lens ((&~), (.=))
import Ruins.Miscellaneous (Name)
import Ruins.Components.Sprites (Sprite)
import Ruins.Components.World (RSystem, opened, currentText, sprite, voiceSound, letterDelay)

say :: Text -> Double -> Maybe Sprite -> Name -> RSystem ()
say text textDelay entityFace voiceName =
  Apecs.set Apecs.global $ mempty &~ do
    opened .= True
    currentText .= text
    sprite .= entityFace
    voiceSound .= voiceName
    letterDelay .= textDelay
