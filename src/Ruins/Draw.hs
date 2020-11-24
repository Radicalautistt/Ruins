{-# Options -fno-warn-unused-top-binds #-}

module Ruins.Draw (drawGame) where

import qualified SDL
import qualified Apecs
import qualified Linear
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Ruins.SDL (mkRectangle, Rect)
import Ruins.Apecs (pattern RXY)
import Control.Lens (view, ix, (^.))
import Ruins.Resources (getResource)
import Ruins.Components (RSystem, Name (..), Frisk (..), Action (..), SpriteSheet (..),
                         sprites, spriteSheet, clips, currentClipIndex, mkName, pattern Renderer)

type Colour = Linear.V4 Word8

friskExtent :: (CInt, CInt)
friskExtent = (60, 92)

black :: Colour
black = Linear.V4 0 0 0 0

white :: Colour
white = Linear.V4 255 255 255 255

pink :: Colour
pink = Linear.V4 255 204 229 0

spriteSheetRow :: Name -> Int -> RSystem Rect
spriteSheetRow spriteSheetName rowIndex = do
  MkSpriteSheet {..} <- getResource sprites spriteSheetName
  let animationClips = _animations ^. ix rowIndex . clips
      currentClip = _animations Vector.! rowIndex ^. currentClipIndex
      defaultRectangle = animationClips Vector.! 0
      maybeRectangle =  animationClips Vector.!? currentClip
  if _animated
     then maybe (pure defaultRectangle) pure maybeRectangle
          else pure defaultRectangle

drawPart :: Name -> Rect -> Rect -> RSystem ()
drawPart spriteSheetName sourceRect targetRect = do
  Renderer renderer <- Apecs.get Apecs.global
  sourceSheet <- view spriteSheet <$> getResource sprites spriteSheetName
  SDL.copy renderer sourceSheet (Just sourceRect) (Just targetRect)

drawFrisk :: RSystem ()
drawFrisk = do
  let friskClip = spriteSheetRow (mkName "frisk")
  moveUp <- friskClip 0
  moveDown <- friskClip 1
  moveLeft <- friskClip 2
  moveRight <- friskClip 3
  Apecs.cmapM_ \ (Frisk, RXY x y, action) -> do
   
    let targetRect = mkRectangle (x, y) friskExtent

    case action of
      MoveUp -> drawPart (mkName "frisk") moveUp targetRect
      MoveDown -> drawPart (mkName "frisk") moveDown targetRect
      MoveLeft -> drawPart (mkName "frisk") moveLeft targetRect
      MoveRight -> drawPart (mkName "frisk") moveRight targetRect

drawGame :: RSystem ()
drawGame = do
  Renderer renderer <- Apecs.get Apecs.global
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= pink
  drawFrisk
  SDL.present renderer
