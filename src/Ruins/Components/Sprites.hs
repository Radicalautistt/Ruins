{-# Options -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleInstances #-}

module Ruins.Components.Sprites where

import qualified SDL
import qualified Apecs.TH as Apecs
import GHC.Int (Int32 (..))
import Foreign.Ptr (nullPtr)
import Unsafe.Coerce (unsafeCoerce)
import Data.Text (Text)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Array.Base as Array
import Data.Array.Unboxed (UArray)
import qualified Data.Array.ST as Array
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Traversable (for)
import Control.Lens (makeLenses)
import Ruins.Extra.SDL (Rect)
import Ruins.Extra.Apecs (makeGlobalComponent)
import Ruins.Miscellaneous (Name)

newtype Sprite = MkSprite (Name, Rect)
  deriving newtype Aeson.FromJSON

-- | This instance is probably awful, I'll try to figure out something better in the future.
instance element ~ Int32 =>
  Aeson.FromJSON (UArray element element) where
  parseJSON = Aeson.withArray "UArray" \ array -> do
    parsedArray <- for array (Aeson.parseJSON @element)
    pure $ Array.runSTUArray do
      mutableArray <- Array.newListArray
        (0, fromIntegral (Vector.length parsedArray - 1)) (Vector.toList parsedArray)
      pure mutableArray

data TileMap = MkTileMap {
  _sourceName :: Name
  , _sourceRectsPath :: FilePath
  , _tileMap :: UArray Int32 Int32
  , _tileWidth :: Int32
  , _tileHeight :: Int32
  , _tileMapWidth :: Int32
  , _tileMapHeight :: Int32
}

instance Aeson.FromJSON TileMap where
  parseJSON = Aeson.withObject "tilemap" \ tileMap -> do
    _sourceName <- tileMap .: "source-name"
    _sourceRectsPath <- tileMap .: "source-rects-path"
    _tileMap <- tileMap .: "tile-map"
    _tileWidth <- tileMap .: "tile-width"
    _tileHeight <- tileMap .: "tile-height"
    _tileMapWidth <- tileMap .: "tile-map-width"
    _tileMapHeight <- tileMap .: "tile-map-height"
    pure MkTileMap {..}

newtype CurrentRoomTexture = MkCurrentRoomTexture (SDL.Texture)
instance Semigroup CurrentRoomTexture where _previous <> next = next
instance Monoid CurrentRoomTexture where mempty = MkCurrentRoomTexture (unsafeCoerce nullPtr)

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

traverse makeGlobalComponent [
  ''CurrentRoomTexture
  ]

concat <$> traverse makeLenses [
  ''TileMap
  , ''Animation
  , ''SpriteSheet
  ]

Apecs.makeMapComponents [
  ''Sprite
  ]
