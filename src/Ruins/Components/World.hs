{-# Options -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components.World (
       RSystem
     , Room (..)
     , Time (..)
     , Rooms (..)
     , Lever (..)
     , TextBox (..)
     , Pressed (..)
     , Boundary (..)
     , QuitGame (..)
     , Resources (..)
     , ResourceMap
     , initRuins
     , sprites
     , fonts
     , sounds
     , music
     , background
     , rooms
     , sprite
     , opened
     , currentText
     , letterDelay
     , voiceSound
     , visibleChunk
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified SDL.Internal.Types as SDL
import Apecs (Has (..), Storage, SystemT (..), explInit)
import qualified Apecs
import qualified Apecs.TH as Apecs
import qualified Apecs.Physics as APhysics
import Foreign.Ptr (nullPtr)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup (Sum (..), Any (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Ruins.Apecs (ManagedSystem, makeGlobalComponent)
import Ruins.Miscellaneous (Name, mkName)
import Ruins.Components.Sprites (Sprite (..), SpriteSheet (..), TileMap (..))
import Ruins.Components.Characters (Frisk, Froggit, Napstablook, InFight, Speed, HealthPoints, Action)
import Control.Lens (makeLenses)
import Control.Monad.Reader (asks)

newtype Time = MkTime Double
  deriving newtype Num
  deriving (Semigroup, Monoid) via Sum Double

data Lever = Lever

-- | The flag component, which tells the game
-- | whether the associated entity was pressed/activated.
-- | Used with levers and buttons.
newtype Pressed = MkPressed Bool

data Room = MkRoom {
  _background :: Either SDL.Texture TileMap
}

newtype Rooms = MkRooms { _rooms :: HashMap Name Room }
  deriving newtype (Semigroup, Monoid)

data TextBox = MkTextBox {
  _sprite :: Maybe Sprite
  , _opened :: Bool
  , _currentText :: Text
  , _letterDelay :: Double
  , _voiceSound :: Name
  , _visibleChunk :: Int
}

instance Semigroup TextBox where _previous <> next = next
instance Monoid TextBox where mempty = MkTextBox Nothing False Text.empty 0.1 (mkName "default-voice") 1

type ResourceMap resource = HashMap Name resource

data Resources = MkResources {
     _sprites :: ResourceMap SpriteSheet
   , _fonts :: ResourceMap Font.Font
   , _sounds :: ResourceMap Mixer.Chunk
   , _music :: ResourceMap Mixer.Music
}

instance Semigroup Resources where
  MkResources {..} <> MkResources sp f s m =
    MkResources
      (_sprites <> sp)
      (_fonts <> f)
      (_sounds <> s)
      (_music <> m)

instance Monoid Resources where
  mempty =
    MkResources HMap.empty HMap.empty HMap.empty HMap.empty

data Boundary = MkBoundary {
     xmax :: Double
   , xmin :: Double
   , ymax :: Double
   , ymin :: Double
}

instance Semigroup Boundary where _previous <> next = next
instance Monoid Boundary where mempty = MkBoundary 0 0 0 0

-- | The Semigroup and Monoid instances for SDL.Window/Renderer
-- | are needed to use them as global apecs components.
instance Semigroup SDL.Window where _a <> b = b
instance Monoid SDL.Window where mempty = SDL.Window nullPtr

instance Semigroup SDL.Renderer where _previous <> next = next
instance Monoid SDL.Renderer where mempty = SDL.Renderer nullPtr

-- | Global flag telling the game whether it should end or not.
newtype QuitGame = MkQuitGame Bool
  deriving (Semigroup, Monoid) via Any

traverse makeGlobalComponent [
  ''Time
  , ''Rooms
  , ''TextBox
  , ''Boundary
  , ''QuitGame
  , ''Resources
  , ''SDL.Window
  , ''SDL.Renderer
  ]

Apecs.makeMapComponents [
    ''Room
  , ''Lever
  , ''Pressed
  ]

Apecs.makeWorld "Ruins" [
  ''Time
  , ''Rooms
  , ''Frisk
  , ''Lever
  , ''Speed
  , ''Action
  , ''Sprite
  , ''TextBox
  , ''InFight
  , ''Pressed
  , ''Froggit
  , ''Boundary
  , ''QuitGame
  , ''Resources
  , ''SDL.Window
  , ''Napstablook
  , ''SDL.Renderer
  , ''HealthPoints
  , ''APhysics.Physics
  ]

concat <$> traverse makeLenses [
  ''Room
  , ''Rooms
  , ''TextBox
  , ''Resources
  ]

type RSystem result = ManagedSystem Ruins result
