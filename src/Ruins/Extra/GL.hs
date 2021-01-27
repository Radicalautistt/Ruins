{-# Language ViewPatterns #-}

module Ruins.Extra.GL where

import qualified SDL
import qualified SDL.Image as Image
import qualified Linear
import qualified Graphics.Rendering.OpenGL as GL
import GHC.Int (Int32)
import Data.Vector (Vector)
import Control.Exception (bracket, bracket_)
import qualified Data.Vector.Storable as SVector
import qualified Data.ByteString as BString
import Foreign.Storable (Storable (..))
import Data.Foldable (for_)
import Control.Monad (void, unless)
import Control.Monad.IO.Class (MonadIO (..))

data Texture = Texture {
  textureWidth :: Int32
  , textureHeight :: Int32
  , textureObject :: GL.TextureObject
} deriving stock (Show, Eq, Ord)

compileShader :: (MonadFail m, MonadIO m) => GL.ShaderType -> FilePath -> m GL.Shader
compileShader shaderType shaderPath = liftIO do
 shader <- GL.createShader shaderType
 shaderData <- BString.readFile shaderPath
 GL.shaderSourceBS shader GL.$= shaderData
 GL.compileShader shader
 shaderCompiled <- GL.get (GL.compileStatus shader)
 unless shaderCompiled do
   reason <- GL.get (GL.shaderInfoLog shader)
   fail ("compileShader: failed to compile a shader.\nreason: " <> reason)

 GL.releaseShaderCompiler
 pure shader

-- | TODO
-- | consider parsing attributes out of a vertex shader file instead of writing them by hand (language-glsl should help).
createProgram :: MonadIO m => Vector GL.Shader -> Vector (String, GL.AttribLocation) -> m GL.Program
createProgram shaders attributes = liftIO do
  program <- GL.createProgram
  for_ shaders (GL.attachShader program)
  for_ attributes \ (name, location) ->
    GL.attribLocation program name GL.$= location

  GL.linkProgram program
  programLinked <- GL.get (GL.linkStatus program)
  unless programLinked do
    reason <- GL.get (GL.programInfoLog program)
    fail ("createProgram: failed to create a program.\nreason: " <> reason)

  pure program

bindTexture2D :: MonadIO m => GL.TextureObject -> m ()
bindTexture2D textureObject =
  GL.textureBinding GL.Texture2D GL.$= Just textureObject

unbindTexture2D :: MonadIO m => m ()
unbindTexture2D =
  GL.textureBinding GL.Texture2D GL.$= Nothing

wrapTexture2D :: MonadIO m => GL.Repetition -> GL.Clamping -> m ()
wrapTexture2D repetition clamping = do
  GL.textureWrapMode target GL.S GL.$= (repetition, clamping)
  GL.textureWrapMode target GL.T GL.$= (repetition, clamping)
  where target = GL.Texture2D

populateBuffer ::
  forall vertex m. (Num vertex, Storable vertex, MonadIO m) =>
  GL.BufferTarget -> SVector.Vector vertex -> GL.BufferUsage -> m ()

populateBuffer target vertices usage = liftIO do
  SVector.unsafeWith vertices \ pointer ->
    case fromIntegral (SVector.length vertices * (sizeOf @vertex 0)) of
      size -> GL.bufferData target GL.$= (size, pointer, usage)

-- | Stolen from:
-- | gitlab.com/dpwiz/exstare/-/blob/master/src/Components/Textures.hs#L81
loadTexture :: MonadIO m => FilePath -> m Texture
loadTexture texturePath = liftIO do
  textureObject <- GL.genObjectName @GL.TextureObject

  bracket (Image.load texturePath) SDL.freeSurface \ source -> do
    sourceParams@
      (Linear.V2 (fromIntegral -> textureWidth)
                 (fromIntegral -> textureHeight)) <- SDL.surfaceDimensions source

    bracket (SDL.createRGBSurface sourceParams SDL.RGBA8888) SDL.freeSurface \ temporary -> do
      void (SDL.surfaceBlit source Nothing temporary Nothing)

      bracket_ (SDL.lockSurface temporary) (SDL.unlockSurface temporary) do
        pixels <- SDL.surfacePixels temporary
        bindTexture2D textureObject
        GL.texture target GL.$= GL.Enabled
        GL.textureFilter target GL.$= ((GL.Nearest, Nothing), GL.Nearest)
        wrapTexture2D GL.Repeated GL.ClampToEdge
        GL.generateMipmap target GL.$= GL.Enabled

        GL.texImage2D target GL.NoProxy 0 GL.RGBA8
          (GL.TextureSize2D textureWidth textureHeight) 0 (GL.PixelData GL.ABGR GL.UnsignedByte pixels)
        GL.textureFilter target GL.$= ((GL.Nearest, Just GL.Nearest), GL.Nearest)

        unbindTexture2D
        pure Texture {..}
        where target = GL.Texture2D
