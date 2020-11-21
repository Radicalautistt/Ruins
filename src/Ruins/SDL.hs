{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}

module Ruins.SDL (
       initSDL
     , quitSDL
     , Rect
     , mkRectangle
     , makeKeyPressed
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Control.Exception (bracket)
import Control.Lens ((&~), (%=))
import Data.Text.Lens (packed)
import Control.Monad.Managed (Managed, managed)
import qualified Language.Haskell.TH as THaskell
import qualified Language.Haskell.TH.Syntax as THaskell

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
  Mixer.closeAudio
  Mixer.quit
  SDL.quit

keyPressed :: SDL.Keycode -> SDL.KeyboardEventData -> Bool
keyPressed key keyboardData =
  SDL.keyboardEventKeyMotion keyboardData == SDL.Pressed
  && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardData) == key

-- | Generate pattern of the form:
-- | pattern PRESSED_KEYNAME :: SDL.EventPayload
-- | pattern PRESSED_KEYNAME <- SDL.KeyboardEvent (keyPressed KeycodeKeyName -> True)
makeKeyPressed :: THaskell.Name -> THaskell.DecsQ
makeKeyPressed keyName = do
  let patternName = THaskell.mkName name
  patternType <- [t| SDL.EventPayload |]
  patternBody <- [p| SDL.KeyboardEvent (keyPressed $(THaskell.conE keyName) -> True) |]

  pure [ THaskell.PragmaD (THaskell.CompleteP [patternName] Nothing)
       , THaskell.PatSynSigD patternName patternType
       , THaskell.PatSynD patternName (THaskell.PrefixPatSyn []) THaskell.Unidir patternBody
       ]
   
  where name = THaskell.showName keyName &~ do
                 packed %= Text.replace "Keycode" "PRESSED_"
                 packed %= Text.toUpper
