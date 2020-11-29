{-# Options -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components (
       RSystem
     , Room (..)
     , Time (..)
     , Tile (..)
     , Rooms (..)
     , Lever (..)
     , Frisk (..)
     , Speed (..)
     , Action (..)
     , Sprite (..)
     , TextBox (..)
     , Pressed (..)
     , TileMap (..)
     , Froggit (..)
     , Boundary (..)
     , QuitGame (..)
     , Resources (..)
     , Animation (..)
     , ResourceMap
     , SpriteSheet (..)
     -- | Hide constructor.
     , Name
     -- | Export smart constructor instead.
     , mkName
     , getName
     , initRuins
     , name
     , tileSolid
     , tileRectangle
     , tileMap
     , sourceName
     , delay
     , clips
     , animated
     , currentClipIndex
     , spriteSheet
     , animations
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
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Semigroup (Sum (..), Any (..))
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Traversable (for)
import System.FilePath.Posix (dropExtension)
import Ruins.SDL (Rect)
import Ruins.Apecs (ManagedSystem, makeGlobalComponent)
import Control.Lens (makeLenses)
import Control.Monad.Reader (asks)

newtype Time = MkTime Double
  deriving newtype Num
  deriving (Semigroup, Monoid) via Sum Double

data Frisk = Frisk

instance Apecs.Component Frisk where
  type Storage Frisk = Apecs.Unique Frisk

data Lever = Lever

-- | The flag component, which tells the game
-- | whether the associated entity was pressed/activated.
-- | Used with levers and buttons.
newtype Pressed = MkPressed Bool

-- | One of the npc's/enemies
-- | that Frisk would encounter in Ruins.
data Froggit = Froggit

newtype Speed = MkSpeed Double
  deriving newtype Num

data Action = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight

newtype HealthPoints = MkHealthPoints Double
  deriving newtype Num

-- | Filename without file extension.
-- | Used as a key for ResourceMap.
newtype Name = MkName { getName :: Text }
  deriving stock Eq
  deriving newtype Hashable

{-# Inline mkName #-}
-- | Smart constructor for names
-- , making sure that extension is dropped.
mkName :: FilePath -> Name
mkName fileName = MkName (Text.pack (dropExtension fileName))

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withText "name" \ text ->
    -- | NOTE: this is dumb
    pure (mkName (Text.unpack text))

newtype Sprite = MkSprite (Name, Rect)

data Tile = MkTile {
     _tileSolid :: Bool
  -- | Position and extent of a tile
  -- , on a parent tile map.
   , _tileRectangle :: Rect
}

instance Aeson.FromJSON Tile where
  parseJSON = Aeson.withObject "tile" \ object -> do
    _tileSolid <- object .: "tile-solid"
    _tileRectangle <- object .: "tile-rectangle"
    pure MkTile {..}

data TileMap = MkTileMap {
  -- | Two-dimensional array of tiles.
     _tileMap :: Array Int (Array Int Tile)
  -- | Name of a source sprite sheet.
   , _sourceName :: Name
}

instance Aeson.FromJSON element => Aeson.FromJSON (Array Int element) where
  parseJSON = Aeson.withArray "array" \ array -> do
    elements <- for array Aeson.parseJSON
    pure $ Array.listArray (0, Vector.length array - 1) (Vector.toList elements)

instance Aeson.FromJSON TileMap where
  parseJSON = Aeson.withObject "tile map" \ object -> do
    _tileMap <- object .: "tile-map"
    _sourceName <- object .: "source-name"
    pure MkTileMap {..}

data Room = MkRoom {
  _background :: Either SDL.Texture TileMap
}

newtype Rooms = MkRooms { _rooms :: HashMap Name Room }
  deriving newtype (Semigroup, Monoid)

data Animation = MkAnimation {
     _name :: Text
   , _delay :: Double
   , _clips :: Vector Rect
   , _currentClipIndex :: Int
}

instance Aeson.FromJSON Animation where
  parseJSON = Aeson.withObject "animation" \ object -> do
    _name <- object .: "name"
    _delay <- object .: "delay"
    _clips <- object .: "clips"      
    let _currentClipIndex = 0

    pure MkAnimation {..}

data SpriteSheet = MkSpriteSheet {
     _animated :: Bool
   , _spriteSheet :: SDL.Texture
   , _animations :: Vector Animation
}

data TextBox = MkTextBox {
  _sprite :: Maybe Sprite
  , _opened :: Bool
  , _currentText :: Text
  , _letterDelay :: Double
  , _voiceSound :: Name
  , _visibleChunk :: Int
}

instance Semigroup TextBox where _previous <> next = next
instance Monoid TextBox where mempty = MkTextBox Nothing False Text.empty 0.3 (mkName "") 1

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
  ''Action
  , ''Room
  , ''Speed
  , ''Lever
  , ''Sprite
  , ''Pressed
  , ''Froggit
  , ''HealthPoints
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
  , ''Pressed
  , ''Froggit
  , ''Boundary
  , ''QuitGame
  , ''Resources
  , ''SDL.Window
  , ''SDL.Renderer
  , ''HealthPoints
  , ''APhysics.Physics
  ]

concat <$> traverse makeLenses [
  ''Room
  , ''Tile
  , ''Rooms
  , ''TileMap
  , ''TextBox
  , ''Animation
  , ''SpriteSheet
  , ''Resources
  ]

type RSystem result = ManagedSystem Ruins result
