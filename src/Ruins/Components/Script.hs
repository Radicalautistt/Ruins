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
import Ruins.Miscellaneous (Name)
import Ruins.Components.Characters (Action (..))
import Ruins.Components.Sprites (Sprite (..))

data Command = Walk Action Double Double Apecs.Entity
   | Say Text Double (Maybe Sprite) Name deriving stock Show

data Script = MkScript {
  _scriptCounter :: Int
  -- ^ Current position in a command vector
  , _scriptData :: Vector Command
} deriving stock Show

instance Apecs.Component Script where
  type Storage Script = Apecs.Map Script

makeLenses ''Script
