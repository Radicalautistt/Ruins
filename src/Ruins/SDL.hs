module Ruins.SDL (
       initSDL
     , quitSDL
     , Rect
     , mkRectangle
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Control.Exception (bracket)
import Control.Monad.Managed (Managed, managed)

type Rect = SDL.Rectangle CInt

mkRectangle :: (CInt, CInt) -> (CInt, CInt) -> Rect
mkRectangle (x, y) (width, height) =
  SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 width height)

withWindow :: Text -> SDL.WindowConfig -> Managed SDL.Window
withWindow windowName windowConfig = managed do
  bracket (SDL.createWindow windowName windowConfig) SDL.destroyWindow

withRenderer :: SDL.Window -> SDL.RendererConfig -> Managed SDL.Renderer
withRenderer window rendererConfig = managed do
  bracket (SDL.createRenderer window (-1) rendererConfig) SDL.destroyRenderer

initSDL :: Managed (SDL.Window, SDL.Renderer)
initSDL = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
 
  window <- withWindow "Ruins" SDL.defaultWindow
  renderer <- withRenderer window SDL.defaultRenderer {
    SDL.rendererType = SDL.AcceleratedVSyncRenderer
  }

  pure (window, renderer)

quitSDL :: IO ()
quitSDL = do
  Font.quit
  Mixer.quit
  SDL.quit
