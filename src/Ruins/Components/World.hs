{-# Options -fno-warn-orphans #-}
{-# Language NamedFieldPuns #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components.World (
       RSystem
     , Ruins
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
     , SoundMuted (..)
     , SoundVolume (..)
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
import qualified Apecs.Physics as Physics
import qualified Linear
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CInt (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Semigroup (Sum (..), Any (..))
import Control.Lens (makeLenses)
import Control.Monad.Reader (asks)
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Components.Script as Script
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

newtype Time = Time Double
  deriving newtype Num
  deriving (Semigroup, Monoid) via Sum Double

-- | One of the items that player can interact with.
data Lever = Lever

-- | The flag component, which tells the game
-- | whether the associated entity was pressed/activated.
-- | Used with levers and buttons.
newtype Pressed = Pressed Bool

-- | Room configuration.
-- | Example config can be found at assets/rooms/debug.json
data Room = Room {
   _roomSize :: Linear.V2 CInt
 , _roomBackground :: Either Sprites.Sprite Sprites.TileMap
 , _roomCameraActive :: Bool
}

instance Semigroup Room where _previous <> next = next
instance Monoid Room where
  mempty = Room Linear.zero (Right (Sprites.TileMap "" "" Misc.emptyUArray 0 0 0 0)) False

deriving anyclass instance Aeson.FromJSON element =>
  Aeson.FromJSON (Linear.V2 element)

instance Aeson.FromJSON Room where
  parseJSON = Aeson.withObject "room configuration" \ room -> do
    _roomSize <- room .: "room-size"
    _roomBackground <- room .: "room-background"
    _roomCameraActive <- room .: "camera-active"
    pure Room {..}

-- | Global text box component.
-- | It is handled by the Ruins.Step.stepTextBox function.
data TextBox = TextBox {
  _sprite :: Maybe Sprites.Sprite
  , _opened :: Bool
  , _currentText :: Text
  , _letterDelay :: Double
  , _voiceSound :: Misc.Name
  , _visibleChunk :: Int
}

instance Semigroup TextBox where _previous <> next = next
instance Monoid TextBox where mempty = TextBox Nothing False Text.empty 0.1 "default-voice" 1

type ResourceMap resource = HashMap Misc.Name resource

data Resources = Resources {
     _sprites :: ResourceMap Sprites.SpriteSheet
   , _fonts :: ResourceMap Font.Font
   , _sounds :: ResourceMap Mixer.Chunk
   , _music :: ResourceMap Mixer.Music
}

instance Semigroup Resources where
  Resources {..} <> Resources sp f s m =
    Resources
      (_sprites <> sp)
      (_fonts <> f)
      (_sounds <> s)
      (_music <> m)

instance Monoid Resources where
  mempty =
    Resources HMap.empty HMap.empty HMap.empty HMap.empty

-- | Room boundary.
-- | Should be deprecated as soon as I implement a collision map.
data Boundary = Boundary {
     xmax :: Double
   , xmin :: Double
   , ymax :: Double
   , ymin :: Double
}

instance Semigroup Boundary where _previous <> next = next
instance Monoid Boundary where mempty = Boundary 0 0 0 0

data Camera = Camera {
  _cameraActive :: Bool
  , _cameraScale :: Double
  , _cameraOffset :: Linear.V2 Double
}

instance Semigroup Camera where
  Camera {..} <> Camera active scale position  =
    Camera (_cameraActive && active) (_cameraScale * scale) (_cameraOffset + position)

instance Monoid Camera where
  mempty = Camera False 1 Linear.zero

-- | The Semigroup and Monoid instances for SDL.Window/Renderer
-- | are needed to use them as global apecs components.
instance Semigroup SDL.Window where _a <> b = b
instance Monoid SDL.Window where mempty = SDL.Window nullPtr

instance Semigroup SDL.Renderer where _previous <> next = next
instance Monoid SDL.Renderer where mempty = SDL.Renderer nullPtr

newtype SoundMuted = SoundMuted Bool
  deriving (Semigroup, Monoid) via Any

newtype SoundVolume = SoundVolume Mixer.Volume

instance Monoid SoundVolume where mempty = SoundVolume 0
instance Semigroup SoundVolume where _previous <> next = next

-- | Global flag telling the game whether it should end or not.
newtype QuitGame = QuitGame Bool
  deriving (Semigroup, Monoid) via Any

traverse EApecs.makeGlobalComponent [
  ''Time
  , ''Camera
  , ''TextBox
  , ''Boundary
  , ''QuitGame
  , ''Resources
  , ''SDL.Window
  , ''SoundMuted
  , ''SoundVolume
  , ''SDL.Renderer
  ]

Apecs.makeMapComponents [
    ''Room
  , ''Lever
  , ''Pressed
  ]

Apecs.makeWorld "Ruins" [
  ''Time
  , ''Lever
  , ''Camera
  , ''TextBox
  , ''Pressed
  , ''Boundary
  , ''QuitGame
  , ''Resources
  , ''SDL.Window
  , ''SoundMuted
  , ''SoundVolume
  , ''SDL.Renderer
  , ''Script.Script
  , ''Physics.Physics

  , ''Characters.Frisk
  , ''Characters.Speed
  , ''Characters.Action
  , ''Characters.InFight
  , ''Characters.Froggit
  , ''Characters.Napstablook
  , ''Characters.HealthPoints

  , ''Sprites.Sprite
  , ''Sprites.Background
  ]

concat <$> traverse makeLenses [
  ''Room
  , ''Camera
  , ''TextBox
  , ''Resources
  ]

type RSystem result = EApecs.ManagedSystem Ruins result
