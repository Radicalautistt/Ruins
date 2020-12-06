{-# Options -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components.World (
       RSystem
     , Room (..)
     , NewRoom (..)
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
     , roomSize
     , cameraActive
     , roomBackground
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
import Ruins.Apecs (ManagedSystem, makeGlobalComponent)
import Ruins.Miscellaneous (Name, mkName, emptyUArray)
import Ruins.Components.Sprites (Sprite (..), SpriteSheet (..), TileMap (..), NewTileMap (..))
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

data NewRoom = MkNewRoom {
   _roomSize :: Linear.V2 CInt
 , _cameraActive :: Bool
 , _roomBackground :: Either Sprite NewTileMap
}

instance Semigroup NewRoom where
  _previous <> next = next

instance Monoid NewRoom where
  mempty = MkNewRoom Linear.zero False (Right (MkNewTileMap emptyUArray (mkName "")))

deriving anyclass instance Aeson.FromJSON element =>
  Aeson.FromJSON (Linear.V2 element)

instance Aeson.FromJSON NewRoom where
  parseJSON = Aeson.withObject "room configuration object" \ object -> do
    _roomSize <- object .: "room-size"
    _cameraActive <- object .: "camera-active"
    _roomBackground <- object .: "room-background"
    pure MkNewRoom {..}

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
  , ''NewRoom
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
  , ''NewRoom
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
  , ''NewRoom
  , ''Rooms
  , ''TextBox
  , ''Resources
  ]

type RSystem result = ManagedSystem Ruins result
