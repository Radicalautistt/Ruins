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
import qualified Ruins.Extra.SDL as ESDL
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Miscellaneous as Misc

data Sprite = Sprite {
  _spriteSheetName :: Misc.Name
  , _spriteRectangle :: ESDL.Rect
} deriving stock Show

instance Aeson.FromJSON Sprite where
  parseJSON = Aeson.withObject "sprite" \ sprite -> do
    _spriteSheetName <- sprite .: "spritesheet-name"
    _spriteRectangle <- sprite .: "sprite-rectangle"
    pure Sprite {..}

instance element ~ Int32 =>
  Aeson.FromJSON (UArray element element) where
  parseJSON = Aeson.withArray "UArray" \ array -> do
    parsedArray <- for array (Aeson.parseJSON @element)
    pure $! Array.runSTUArray do
      mutableArray <- Array.newListArray
        (0, fromIntegral (Vector.length parsedArray - 1)) (Vector.toList parsedArray)
      pure mutableArray

data TileMap = TileMap {
  _sourceName :: Misc.Name
  , _sourceRectsPath :: FilePath
  , _tileMap :: UArray Int32 Int32
  , _tileWidth :: Int32
  , _tileHeight :: Int32
  , _tileMapWidth :: Int32
  , _tileMapHeight :: Int32
}

instance Aeson.FromJSON TileMap where
  parseJSON = Aeson.withObject "tilemap data" \ tileMap -> do
    _sourceName <- tileMap .: "source-name"
    _sourceRectsPath <- tileMap .: "source-rects-path"
    _tileMap <- tileMap .: "tile-map"
    _tileWidth <- tileMap .: "tile-width"
    _tileHeight <- tileMap .: "tile-height"
    _tileMapWidth <- tileMap .: "tile-map-width"
    _tileMapHeight <- tileMap .: "tile-map-height"
    pure TileMap {..}

data Background = Background {
  _backgroundTexture :: SDL.Texture
  , _backgroundBoundary :: (Double, Double, Double, Double)
  , _backgroundRectangle :: ESDL.Rect
}

instance Semigroup Background where _previous <> next = next
instance Monoid Background where mempty = Background (unsafeCoerce nullPtr) (0, 0, 0, 0) ESDL.unitRectangle

data Animation = Animation {
     _animationName :: Text
   , _animationDelay :: Double
   , _animationClips :: Vector ESDL.Rect
   , _currentClipIndex :: Int
}

instance Aeson.FromJSON Animation where
  parseJSON = Aeson.withObject "animation data" \ animation -> do
    _animationName <- animation .: "name"
    _animationDelay <- animation .: "delay"
    _animationClips <- animation .: "clips"
    let _currentClipIndex = 0

    pure Animation {..}

data SpriteSheet = SpriteSheet {
     _animated :: Bool
   , _spriteSheet :: SDL.Texture
   , _animations :: Vector Animation
}

traverse EApecs.makeGlobalComponent [
  ''Background
  ]

concat <$> traverse makeLenses [
  ''Sprite
  , ''TileMap
  , ''Animation
  , ''Background
  , ''SpriteSheet
  ]

Apecs.makeMapComponents [
  ''Sprite
  ]
