module Ruins.Extra.GL where

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vector (Vector)
import qualified Data.Vector.Storable as SVector
import qualified Data.ByteString as BString
import Foreign.Storable (Storable (..))
import Data.Foldable (for_)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))

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

populateBuffer ::
  forall vertex m. (Num vertex, Storable vertex, MonadIO m) =>
  GL.BufferTarget -> SVector.Vector vertex -> GL.BufferUsage -> m ()

populateBuffer target vertices usage = liftIO do
  SVector.unsafeWith vertices \ pointer ->
    case fromIntegral (SVector.length vertices * (sizeOf @vertex 0)) of
      size -> GL.bufferData target GL.$= (size, pointer, usage)
