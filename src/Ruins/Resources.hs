{-# Language RankNTypes #-}
{-# Language ViewPatterns #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}

module Ruins.Resources (
       loadRoom
     , getResource
     , loadResources
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified Apecs
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BString
import qualified Data.Text.Short as SText
import qualified Data.Array.IArray as Array
import Foreign.C.Types (CInt (..))
import Data.Foldable (for_)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HMap
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import Control.Exception (bracket)
import qualified Control.Concurrent.Async as Async
import Control.Lens (Lens', set, view, over, (^.), (&), (&~), (.=))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed)
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Extra.SDL as ESDL
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

mkAssetPath :: FilePath -> FilePath
mkAssetPath = (</>) "assets"

roomsPath :: FilePath
roomsPath = mkAssetPath "rooms"

spritesPath :: FilePath
spritesPath = mkAssetPath "sprites"

fontsPath :: FilePath
fontsPath = mkAssetPath "fonts"

soundsPath :: FilePath
soundsPath = mkAssetPath "sounds"

musicPath :: FilePath
musicPath = mkAssetPath "music"

animationsPath :: FilePath
animationsPath = mkAssetPath "animations"

class ManagedResource resource where
  manageResource :: FilePath -> World.RSystem resource

{-# Inline withResource #-}
withResource :: (FilePath -> IO resource) -> (resource -> IO ()) -> FilePath -> World.RSystem resource
withResource load free filePath = managed do
  bracket (Async.withAsync (load filePath) Async.wait) free

instance ManagedResource Font.Font where
  {-# Inline manageResource #-}
  -- | The font size can be fixed because there is only two fonts
  -- , and both of them have the same recommended size, which is 13.
  manageResource fileName = withResource (`Font.load` 13) Font.free (fontsPath </> fileName)

instance ManagedResource Mixer.Chunk where
  {-# Inline manageResource #-}
  manageResource fileName = withResource Mixer.load Mixer.free (soundsPath </> fileName)

instance ManagedResource Mixer.Music where
  {-# Inline manageResource #-}
  manageResource fileName = withResource Mixer.load Mixer.free (musicPath </> fileName)

instance ManagedResource SDL.Texture where
  {-# Inline manageResource #-}
  manageResource filePath = do
    renderer <- Apecs.get Apecs.global
   
    let loadTexture fileName = do
          sourceSurface <- SDL.loadBMP (spritesPath </> fileName)
          texture <- SDL.createTextureFromSurface renderer sourceSurface
          SDL.freeSurface sourceSurface
          pure texture

    withResource loadTexture SDL.destroyTexture filePath

loadAnimations :: World.RSystem ()
loadAnimations = do
  animationFiles <- liftIO (Vector.fromList <$> listDirectory animationsPath)
  results <- liftIO $ Async.forConcurrently animationFiles \ fileName -> do
               fileContents <- BString.readFile (animationsPath </> fileName)
               either fail (pure . (Misc.mkName fileName, ))
                 (Aeson.eitherDecodeStrict' @(Vector Sprites.Animation) fileContents)

  for_ results \ (name, animationVector) ->
    let insertAnimations = set Sprites.animations animationVector
    in Apecs.modify Apecs.global
        (over World.sprites (HMap.update (Just . insertAnimations) name))

-- | Load room from the given configuration file, and set Ruins.Components.Sprites.Background
-- | to either be a solid background texture, or the result of rendering a tile map
-- | into an empty texture. In the latter case, the Background serves as some kind of a buffer
-- | to which we can render a tile map once, and reuse it after as much as we want, instead of
-- | rerendering it every frame. Perhaps, it is not the best approach, and yet it saves a ton of draw calls
-- , which in turn saves our "resources budget".
loadRoom :: FilePath -> World.RSystem ()
loadRoom roomFile = do
  World.Room {..} <-
    Misc.decodeJSON @World.Room =<< liftIO (BString.readFile (roomsPath </> roomFile))

  Apecs.global Apecs.$~ \ camera -> camera &~ do
    World.cameraActive .= _roomCameraActive
    World.cameraViewport .= _roomCameraViewport

  World.SoundMuted muted <- Apecs.get Apecs.global
  unless muted do
    Mixer.playMusic Mixer.Forever =<<
      getResource World.music _roomMusic

  Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
    friskEntity Apecs.$=
      (EApecs.mkPosition `uncurry` _roomPlayerInitPosition, _roomPlayerInitAction)

  Apecs.global Apecs.$= _roomDivarication

  renderer <- Apecs.get Apecs.global
  -- | Some backgrounds in Undertale are plain images, thus we need to handle them properly.
  case _roomBackground of
    Left Sprites.Sprite {..} -> do
      (view Sprites.spriteSheet -> source) <-
        getResource World.sprites _spriteSheetName
      target <- SDL.createTexture renderer
        SDL.RGB888 SDL.TextureAccessTarget _roomSize

      SDL.rendererRenderTarget renderer SDL.$= Just target
      ESDL.copyTexture source renderer (Just _spriteRectangle) Nothing

      SDL.rendererRenderTarget renderer SDL.$= Nothing
      Sprites.Background {..} <- Apecs.get Apecs.global
      SDL.destroyTexture _backgroundTexture
      Apecs.global Apecs.$= Sprites.Background target _roomBoundary _roomBackgroundRectangle

    -- | While others are made of tiles.
    Right Sprites.TileMap {..} -> do
      sourceRects <-
        Misc.decodeJSON @(Vector ESDL.Rect) =<< liftIO (BString.readFile _sourceRectsPath)

      (view Sprites.spriteSheet -> source) <-
        getResource World.sprites _sourceName
       
      target <- SDL.createTexture renderer
        SDL.RGB888 SDL.TextureAccessTarget _roomSize

      SDL.rendererRenderTarget renderer SDL.$= Just target

      for_ [0.. _tileMapHeight - 1] \ row ->
        for_ [0.. _tileMapWidth - 1] \ column ->
          case _tileMap Array.! (row * _tileMapWidth + column) of
            tile -> case sourceRects Vector.! (fromIntegral tile - 1) of
              tileRect ->
                ESDL.copyTexture source renderer (Just tileRect)
                  (Just (ESDL.mkRectangle (CInt (column * _tileWidth),
                                           CInt (row * _tileHeight)) (CInt _tileWidth, CInt _tileHeight)))
                 
      SDL.rendererRenderTarget renderer SDL.$= Nothing
      Sprites.Background {..} <- Apecs.get Apecs.global
      SDL.destroyTexture _backgroundTexture
      Apecs.global Apecs.$= Sprites.Background target _roomBoundary _roomBackgroundRectangle

loadResources :: World.RSystem ()
loadResources = do
  -- | Doesn't work if there is an inner directory
  -- | instead of a file. But this will do for the time being.
  spriteFiles <- contentsOf spritesPath
  fontFiles <- contentsOf fontsPath
  soundFiles <- contentsOf soundsPath
  musicFiles <- contentsOf musicPath

  for_ spriteFiles insertSprite
  for_ fontFiles (insertResource World.fonts)
  for_ soundFiles (insertResource World.sounds)
  for_ musicFiles (insertResource World.music)

  loadAnimations

  where contentsOf = liftIO . listDirectory
        insertSprite spriteName = do
          texture <- manageResource spriteName
          let sprite = Sprites.SpriteSheet False texture Vector.empty
          Apecs.modify Apecs.global
            (over World.sprites (HMap.insert (Misc.mkName spriteName) sprite))

        insertResource resourceLens resourceName = do
          resource <- manageResource resourceName
         
          Apecs.modify Apecs.global
            (over resourceLens (HMap.insert (Misc.mkName resourceName) resource))

{-# Inline getResource #-}
-- | Asynchronously get a desired resource by providing a lens to the field where a
-- | said resource is stored, and also its name.
-- | Example usage: do
-- |   megalovania <- getResource music "megalovania"
-- |   Mixer.playMusic Mixer.Forever megalovania
getResource :: Lens' World.Resources (World.ResourceMap resource) -> Misc.Name -> World.RSystem resource
getResource fieldLens resourceName = do
  resources <- Apecs.get Apecs.global
  let resource = do
        let maybeResource = resources ^. fieldLens & HMap.lookup resourceName
            errorMessage = "getResource: " <> SText.unpack (Misc.getName resourceName) <> " hasn't been found."
        maybe (fail errorMessage) pure maybeResource

  liftIO (Async.withAsync resource Async.wait)
