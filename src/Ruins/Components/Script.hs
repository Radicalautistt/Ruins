{-# Language TemplateHaskell #-}

module Ruins.Components.Script (
     -- * components
       Command (..)
     , Script (..)
     -- * lenses
     , scriptData
     , scriptCounter
     ) where

import qualified Apecs
import Data.Text (Text)
import Data.Vector (Vector)
import Control.Lens (makeLenses)
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

data Command = Walk Characters.Action Double Double Apecs.Entity
   | Say Text Double (Maybe Sprites.Sprite) Misc.Name deriving stock Show

data Script = Script {
  _scriptCounter :: Int
  -- ^ Current position in a command vector
  , _scriptData :: Vector Command
} deriving stock Show

instance Apecs.Component Script where
  type Storage Script = Apecs.Map Script

makeLenses ''Script
