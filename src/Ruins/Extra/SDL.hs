{-# Options -fno-warn-orphans #-}
{-# Language RankNTypes #-}
{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}
{-# Language StandaloneDeriving #-}

-- | This module provides helper functions
-- for working with sdl2.
module Ruins.Extra.SDL (
     -- * utils
       initSDL
     , quitSDL
     , Rect
     , Color
     , mkRectangle
     -- * lenses
     , rectExtent
     , rectPosition
     -- * TH
     , makeKeyPressed
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified Linear
import qualified Linear.Affine as Linear
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Control.Exception (bracket)
import Control.Lens (Lens', lens, set, over, ix, (&~), (%=), (^.))
import Data.Text.Lens (packed)
import Control.Monad.Managed (Managed, managed)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

type Color = Linear.V4 Word8

type Rect = SDL.Rectangle CInt

deriving stock instance Generic CInt
deriving anyclass instance Aeson.FromJSON CInt

instance (Num value, Aeson.FromJSON value) => Aeson.FromJSON (SDL.Rectangle value) where
  parseJSON = Aeson.withArray "SDL rectangle" \ array -> do
    [x, y, width, height] <- Aeson.parseJSON @[value] (Aeson.Array array)
    pure (mkRectangle (x, y) (width, height))

{-# Specialize mkRectangle :: (CInt, CInt) -> (CInt, CInt) -> Rect #-}
mkRectangle :: Num value => (value, value) -> (value, value) -> SDL.Rectangle value
mkRectangle (x, y) (width, height) =
  SDL.Rectangle (Linear.P (Linear.V2 x y)) (Linear.V2 width height)

-- | Lenses for SDL.Rectangles. Example usage:
-- | ghci> mkRectangle (10, 20) (30, 40) & rectPosition _x +~ 100
-- | ghci> Rectangle (P (V2 110 20)) (V2 30 40)
rectPosition :: Lens' (Linear.V2 CInt) CInt -> Lens' Rect CInt
rectPosition axisLens = lens getter setter
  where getter (SDL.Rectangle (Linear.P position) _) = position ^. axisLens
        setter (SDL.Rectangle (Linear.P position) extent) newAxisValue =
          SDL.Rectangle (Linear.P (set axisLens newAxisValue position)) extent

rectExtent :: Lens' (Linear.V2 CInt) CInt -> Lens' Rect CInt
rectExtent axisLens = lens getter setter
  where getter (SDL.Rectangle _ extent) = extent ^. axisLens
        setter (SDL.Rectangle position extent) newExtentValue =
          SDL.Rectangle position (set axisLens newExtentValue extent)

withWindow :: Text -> SDL.WindowConfig -> Managed SDL.Window
withWindow windowName windowConfig = managed do
  bracket (SDL.createWindow windowName windowConfig) SDL.destroyWindow

withRenderer :: SDL.Window -> SDL.RendererConfig -> Managed SDL.Renderer
withRenderer window config = managed do
  bracket (SDL.createRenderer window (-1) config) SDL.destroyRenderer

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig SDL.AcceleratedVSyncRenderer True

initSDL :: Managed (SDL.Window, SDL.Renderer)
initSDL = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
 
  window <- withWindow "Ruins" SDL.defaultWindow
  renderer <- withRenderer window rendererConfig

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
makeKeyPressed :: TH.Name -> TH.DecsQ
makeKeyPressed keyName = do
  let patternName = TH.mkName name
  patternType <- [t| SDL.EventPayload |]
  patternBody <- [p| SDL.KeyboardEvent (keyPressed $(TH.conE keyName) -> True) |]

  pure [ TH.PragmaD (TH.CompleteP [patternName] Nothing)
       , TH.PatSynSigD patternName patternType
       , TH.PatSynD patternName (TH.PrefixPatSyn []) TH.Unidir patternBody
       ]
   
  where name = TH.showName keyName &~ do
                 packed %= Text.replace "Keycode" "PRESSED_"
                 packed %= Text.intercalate "_" . over (ix 1) Text.toUpper . Text.splitOn "_"
