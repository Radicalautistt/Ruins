{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language TupleSections #-}

module Ruins.Resources (
       getResource
     , loadResources
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified Apecs
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BString
import qualified Data.ByteString.Lazy as LBString
import qualified Data.Text as Text
import Data.Foldable (for_)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HMap
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import Control.Exception (bracket)
import qualified Control.Concurrent.Async as Async
import Control.Lens (Lens', set, over, to, (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed)
import Ruins.Components (RSystem, SpriteSheet (..), Resources (..), Name, getName, Animation (..),
                         ResourceMap, animations, sprites, fonts, sounds, music, mkName, pattern Renderer,
                         TileMap (..), Room (..), rooms)

mkAssetPath :: FilePath -> FilePath
mkAssetPath = (</>) "assets"

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
  manageResource :: FilePath -> RSystem resource

{-# Inline withResource #-}
withResource :: (FilePath -> IO resource) -> (resource -> IO ()) -> FilePath -> RSystem resource
withResource load free filePath = managed do
  bracket (Async.withAsync (load filePath) Async.wait) free

instance ManagedResource Font.Font where
  {-# Inline manageResource #-}
  -- | The font size can be fixed because there is only two fonts
  -- , and they have the same recommended size, which is 10.
  manageResource fileName = withResource (`Font.load` 10) Font.free (fontsPath </> fileName)

instance ManagedResource Mixer.Chunk where
  {-# Inline manageResource #-}
  manageResource fileName = withResource Mixer.load Mixer.free (soundsPath </> fileName)

instance ManagedResource Mixer.Music where
  {-# Inline manageResource #-}
  manageResource fileName = withResource Mixer.load Mixer.free (musicPath </> fileName)

instance ManagedResource SDL.Texture where
  {-# Inline manageResource #-}
  manageResource filePath = do
    Renderer renderer <- Apecs.get Apecs.global
   
    let loadTexture fileName = do
          sourceSurface <- SDL.loadBMP (spritesPath </> fileName)
          texture <- SDL.createTextureFromSurface renderer sourceSurface
          SDL.freeSurface sourceSurface
          pure texture

    withResource loadTexture SDL.destroyTexture filePath

loadAnimations :: RSystem ()
loadAnimations = do
  animationFiles <- liftIO (Vector.fromList <$> listDirectory animationsPath)
  results <- liftIO $ Async.forConcurrently animationFiles \ fileName -> do
               fileContents <- LBString.readFile (animationsPath </> fileName)
               either fail (pure . (mkName fileName, ))
                 (Aeson.eitherDecode @(Vector Animation) fileContents)

  for_ results \ (name, animationVector) ->
    let insertAnimations = set animations animationVector
    in Apecs.modify Apecs.global
        (over sprites (HMap.update (Just . insertAnimations) name))

loadRooms :: RSystem ()
loadRooms = do
  roomFiles <- liftIO (listDirectory (mkAssetPath "rooms"))
  results <- liftIO $ Async.forConcurrently roomFiles \ fileName -> do
    fileContents <- BString.readFile (mkAssetPath "rooms" </> fileName)
    either fail (pure . (mkName fileName, ))
      (Aeson.eitherDecodeStrict' @TileMap fileContents)

  for_ results \ (name, tileMap) ->
    Apecs.modify Apecs.global
      (over rooms (HMap.insert name (MkRoom (Right tileMap))))

loadResources :: RSystem ()
loadResources = do
  -- | Doesn't work if there is an inner directory
  -- | instead of a file. But this will do for the time being.
  spriteFiles <- contentsOf spritesPath
  fontFiles <- contentsOf fontsPath
  soundFiles <- contentsOf soundsPath
  musicFiles <- contentsOf musicPath

  for_ spriteFiles insertSprite
  for_ fontFiles (insertResource fonts)
  for_ soundFiles (insertResource sounds)
  for_ musicFiles (insertResource music)

  loadRooms
  loadAnimations

  where contentsOf = liftIO . listDirectory
        insertSprite spriteName = do
          texture <- manageResource spriteName
          let sprite = MkSpriteSheet False texture Vector.empty
          Apecs.modify Apecs.global
            (over sprites (HMap.insert (mkName spriteName) sprite))

        -- | Eww, code duplication...
        insertResource resourceLens resourceName = do
          resource <- manageResource resourceName
         
          Apecs.modify Apecs.global
            (over resourceLens (HMap.insert (mkName resourceName) resource))

getResource :: Lens' Resources (ResourceMap resource) -> Name -> RSystem resource
getResource fieldLens resourceName = do
  resources <- Apecs.get Apecs.global
  let resource = do
        let maybeResource = resources ^. fieldLens . to (HMap.lookup resourceName)
            -- | Consider writing a custom MonadFail class with Text instead of String.
            errorMessage = Text.unpack ("getResource: " <> getName resourceName <> " hasn't been found.")
        maybe (fail errorMessage) pure maybeResource

  -- | Perhaps I should switch to async-lifted.
  liftIO (Async.withAsync resource Async.wait)
