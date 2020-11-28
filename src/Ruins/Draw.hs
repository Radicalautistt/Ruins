{-# Options -fno-warn-unused-top-binds #-}
{-# Language FlexibleContexts #-}

module Ruins.Draw (drawGame) where

import qualified SDL
import qualified Apecs
import qualified Linear
import qualified Data.Vector as Vector
import Data.Bool (bool)
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Data.HashMap.Strict as HMap
import Foreign.C.Types (CInt)
import Ruins.SDL (mkRectangle, Rect)
import Ruins.Apecs (pattern RXY)
import Control.Lens (ifor_, view, ix, (^.))
import Unsafe.Coerce (unsafeCoerce)
import Ruins.Resources (getResource)
import Ruins.Components (RSystem, Name (..), Frisk (..), Action (..), SpriteSheet (..), Rooms (..),
                         Room (..), sprites, spriteSheet, clips, currentClipIndex, mkName, pattern Renderer,
                         TileMap (..), Lever (..), Pressed (..), Froggit (..), Sprite (..), tileRectangle)

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
      maybeRectangle = animationClips Vector.!? currentClip
  if _animated
     then maybe (pure defaultRectangle) pure maybeRectangle
          else pure defaultRectangle

drawPart :: SDL.Renderer -> Name -> Rect -> Rect -> RSystem ()
drawPart renderer spriteSheetName sourceRect targetRect = do
  sourceSheet <- view spriteSheet <$> getResource sprites spriteSheetName
  SDL.copy renderer sourceSheet (Just sourceRect) (Just targetRect)

drawLogo :: SDL.Renderer -> RSystem ()
drawLogo renderer = do
  logoSprite <- view spriteSheet <$> getResource sprites (mkName "logo")
  SDL.copy renderer logoSprite Nothing
    (Just do mkRectangle (380, 30) (600, 120))

drawLevers :: SDL.Renderer -> RSystem ()
drawLevers renderer = do
  Apecs.cmapM_ \ (Lever, MkPressed pressed, RXY x y, MkSprite (name, rect)) -> do
    let sourceX = bool 45 53 pressed
    drawPart renderer name rect
      (mkRectangle (x - 5, y - 25) (30, 20))
    drawPart renderer (mkName "ruins-tiles") (mkRectangle (sourceX, 1038) (7, 15))
      (mkRectangle (x, y) (20, 41))

drawRoom :: SDL.Renderer -> Name -> RSystem ()
drawRoom renderer roomName = do
  MkRooms rooms <- Apecs.get Apecs.global
  let maybeRoom = HMap.lookup roomName rooms
      errorMessage = Text.unpack ("drawRoom: no such room " <> getName roomName)
  maybe (fail errorMessage) draw maybeRoom
  drawLogo renderer
  where draw MkRoom {..} = do
          let Right MkTileMap {..} = _background
          sourceSheet <- view spriteSheet <$> getResource sprites _sourceName
          ifor_ _tileMap \ rowIndex row ->
            ifor_ row \ columnIndex column ->
              SDL.copy renderer sourceSheet (Just (column ^. tileRectangle))
                (Just (mkRectangle (unsafeCoerce (columnIndex * 60, rowIndex * 40)) (60,40)))

drawFrisk :: SDL.Renderer -> RSystem ()
drawFrisk renderer = do
  let friskClip = spriteSheetRow (mkName "frisk")
  moveUp <- friskClip 0
  moveDown <- friskClip 1
  moveLeft <- friskClip 2
  moveRight <- friskClip 3
  Apecs.cmapM_ \ (Frisk, RXY x y, action) -> do
   
    let targetRect = mkRectangle (x, y) friskExtent
        draw rect = drawPart renderer (mkName "frisk") rect targetRect

    case action of
      MoveUp -> draw moveUp
      MoveDown -> draw moveDown
      MoveLeft -> draw moveLeft
      MoveRight -> draw moveRight

drawFroggits :: SDL.Renderer -> RSystem ()
drawFroggits renderer = do
  sourceRect <- spriteSheetRow (mkName "froggit") 0
  Apecs.cmapM_ \ (Froggit, RXY x y) ->
    drawPart renderer (mkName "froggit")
      sourceRect
      (mkRectangle (x, y) (59, 60))

drawGame :: RSystem ()
drawGame = do
  Renderer renderer <- Apecs.get Apecs.global
  SDL.clear renderer
  drawRoom renderer (mkName "debug-room")
  drawLevers renderer
  drawFroggits renderer
  drawFrisk renderer
  SDL.present renderer
