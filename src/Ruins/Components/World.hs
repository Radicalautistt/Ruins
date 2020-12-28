{-# Options -fno-warn-orphans #-}
{-# Language NamedFieldPuns #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components.World (
       RSystem
  -- * components
     , Room (..)
     , Time (..)
     , Lever (..)
     , Camera (..)
     , TextBox (..)
     , Pressed (..)
     , Boundary (..)
     , QuitGame (..)
     , Resources (..)
     , ResourceMap
  -- * world initialization
     , initRuins
  -- * lenses
     , sprites
     , fonts
     , sounds
     , music
     , sprite
     , opened
     , currentText
     , letterDelay
     , voiceSound
     , visibleChunk
     , cameraActive
     , cameraOffset
     , cameraScale
     , roomSize
     , roomBackground
     , roomCameraActive
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified SDL.Internal.Types as SDL
import Apecs (Has (..), Storage, SystemT (..), explInit)
import qualified Apecs
import qualified Apecs.TH as Apecs
import qualified Apecs.Physics as APhysics
import qualified Linear
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CInt (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup (Sum (..), Any (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Ruins.Extra.Apecs (ManagedSystem, makeGlobalComponent)
import Ruins.Miscellaneous (Name, emptyUArray)
import Ruins.Components.Script (Script (..))
import Ruins.Components.Sprites (Sprite (..), SpriteSheet (..), TileMap (..), CurrentRoomTexture (..))
import Ruins.Components.Characters (Frisk, Froggit, Napstablook, InFight, Speed, HealthPoints, Action)
import Control.Lens (makeLenses)
import Control.Monad.Reader (asks)

newtype Time = MkTime Double
  deriving newtype Num
  deriving (Semigroup, Monoid) via Sum Double

-- | One of the items that player can interact with.
data Lever = Lever

-- | The flag component, which tells the game
-- | whether the associated entity was pressed/activated.
-- | Used with levers and buttons.
newtype Pressed = MkPressed Bool

-- | Room configuration.
-- | Example config can be found at assets/rooms/debug.json
data Room = MkRoom {
   _roomSize :: Linear.V2 CInt
 , _roomBackground :: Either Sprite TileMap
 , _roomCameraActive :: Bool
}

instance Semigroup Room where _previous <> next = next
instance Monoid Room where
  mempty = MkRoom Linear.zero (Right (MkTileMap "" "" emptyUArray 0 0 0 0)) False

deriving anyclass instance Aeson.FromJSON element =>
  Aeson.FromJSON (Linear.V2 element)

instance Aeson.FromJSON Room where
  parseJSON = Aeson.withObject "room configuration" \ room -> do
    _roomSize <- room .: "room-size"
    _roomBackground <- room .: "room-background"
    _roomCameraActive <- room .: "camera-active"
    pure MkRoom {..}

-- | Global text box component.
-- | It is handled by the Ruins.Step.stepTextBox function.
data TextBox = MkTextBox {
  _sprite :: Maybe Sprite
  , _opened :: Bool
  , _currentText :: Text
  , _letterDelay :: Double
  , _voiceSound :: Name
  , _visibleChunk :: Int
}

instance Semigroup TextBox where _previous <> next = next
instance Monoid TextBox where mempty = MkTextBox Nothing False Text.empty 0.1 "default-voice" 1

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

-- | Room boundary.
-- | Should be deprecated as soon as I implement a collision map.
data Boundary = MkBoundary {
     xmax :: Double
   , xmin :: Double
   , ymax :: Double
   , ymin :: Double
}

instance Semigroup Boundary where _previous <> next = next
instance Monoid Boundary where mempty = MkBoundary 0 0 0 0

data Camera = MkCamera {
  _cameraActive :: Bool
  , _cameraScale :: Double
  , _cameraOffset :: Linear.V2 Double
}

instance Semigroup Camera where
  MkCamera {..} <> MkCamera active scale position  =
    MkCamera (_cameraActive && active) (_cameraScale * scale) (_cameraOffset + position)

instance Monoid Camera where
  mempty = MkCamera False 1 Linear.zero

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
  , ''Camera
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
  , ''Frisk
  , ''Lever
  , ''Speed
  , ''Script
  , ''Camera
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
  , ''CurrentRoomTexture
  ]

concat <$> traverse makeLenses [
  ''Room
  , ''Camera
  , ''TextBox
  , ''Resources
  ]

type RSystem result = ManagedSystem Ruins result
