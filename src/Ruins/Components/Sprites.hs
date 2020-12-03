{-# Options -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}

module Ruins.Components.Sprites where

import qualified SDL
import qualified Apecs.TH as Apecs
import Data.Text (Text)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Traversable (for)
import Control.Lens (makeLenses)
import Ruins.SDL (Rect)
import Ruins.Miscellaneous (Name)

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

data Animation = MkAnimation {
     _animationName :: Text
   , _animationDelay :: Double
   , _animationClips :: Vector Rect
   , _currentClipIndex :: Int
}

instance Aeson.FromJSON Animation where
  parseJSON = Aeson.withObject "animation" \ object -> do
    _animationName <- object .: "name"
    _animationDelay <- object .: "delay"
    _animationClips <- object .: "clips"
    let _currentClipIndex = 0

    pure MkAnimation {..}

data SpriteSheet = MkSpriteSheet {
     _animated :: Bool
   , _spriteSheet :: SDL.Texture
   , _animations :: Vector Animation
}

concat <$> traverse makeLenses [
  ''Tile
  , ''TileMap
  , ''Animation
  , ''SpriteSheet
  ]

Apecs.makeMapComponents [
  ''Sprite
  ]
