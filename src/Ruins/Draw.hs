{-# Options -fno-warn-unused-top-binds #-}
{-# Language FlexibleContexts #-}

module Ruins.Draw (drawGame) where

import qualified SDL
import qualified SDL.Font as Font
import qualified Apecs
import qualified Linear
import qualified Data.Vector as Vector
import Data.Bool (bool)
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Data.HashMap.Strict as HMap
import Foreign.C.Types (CInt)
import Ruins.SDL (mkRectangle, rectPosition, rectExtent, Rect)
import Ruins.Apecs (pattern RXY)
import Control.Lens (ifor_, view, ix, (^.), (&~), (-=), (+=))
import Control.Monad (when, unless)
import Unsafe.Coerce (unsafeCoerce)
import Ruins.Resources (getResource)
import Ruins.Components (RSystem, Name (..), Frisk (..), Action (..), SpriteSheet (..), Rooms (..),
                         Room (..), sprites, spriteSheet, clips, currentClipIndex, mkName,
                         TileMap (..), Lever (..), Pressed (..), Froggit (..), Sprite (..),
                         TextBox (..), InFight (..), Napstablook (..), fonts, tileRectangle)

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

drawDoor :: SDL.Renderer -> Rect -> RSystem ()
drawDoor renderer targetRect =
  drawPart renderer (mkName "ruins-tiles") (mkRectangle (567, 93) (101, 78))
    targetRect

drawRectangle :: SDL.Renderer -> Rect -> Colour -> Colour -> RSystem ()
drawRectangle renderer rectangle foreground background = do
  SDL.rendererDrawColor renderer SDL.$= background
  renderer `SDL.fillRect` Just rectangle
  let innerRectangle = rectangle &~ do
        rectExtent Linear._x -= 20
        rectExtent Linear._y -= 20

        rectPosition Linear._x += 10
        rectPosition Linear._y += 10

  unless (foreground == background) do
    SDL.rendererDrawColor renderer SDL.$= foreground
    renderer `SDL.fillRect` Just innerRectangle

drawTextBox :: SDL.Renderer -> RSystem ()
drawTextBox renderer = do
  MkTextBox {..} <- Apecs.get Apecs.global
  when _opened do
    drawRectangle renderer (mkRectangle (350, 500) (700, 200)) black white
    dialogueFont <- getResource fonts (mkName "dialogue")
    tempSurface <- Font.solid dialogueFont white (Text.take _visibleChunk _currentText)
    textTexture <- SDL.createTextureFromSurface renderer tempSurface
    SDL.freeSurface tempSurface
    let textWidth = unsafeCoerce (_visibleChunk * 13)
    SDL.copy renderer textTexture Nothing (Just do mkRectangle (370, 520) (textWidth, 50))
    SDL.destroyTexture textTexture
   
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
                (Just (mkRectangle (unsafeCoerce (columnIndex * 20 * 2, rowIndex * 20 * 2)) (20 * 2,20 * 2)))

drawFrisk :: SDL.Renderer -> RSystem ()
drawFrisk renderer = Apecs.cmapM_ \ (Frisk, RXY x y, action, MkInFight inFight) -> do
  if inFight
     then drawPart renderer (mkName "frisk")
            (mkRectangle (99, 0) (16, 16))
            (mkRectangle (x, y) (20, 20))
          else do let friskClip = spriteSheetRow (mkName "frisk")
                  moveUp <- friskClip 0
                  moveDown <- friskClip 1
                  moveLeft <- friskClip 2
                  moveRight <- friskClip 3

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

drawNapstablook :: SDL.Renderer -> RSystem ()
drawNapstablook renderer =
  Apecs.cmapM_ \ (Napstablook, RXY x y, MkInFight inFight) ->
    if not inFight
       then drawPart renderer (mkName "napstablook")
              (mkRectangle (15, 121) (33, 17))
              (mkRectangle (x, y) (99, 51))
            else do stareRect <- spriteSheetRow (mkName "napstablook") 0
                    drawPart renderer (mkName "napstablook")
                      stareRect (mkRectangle (x, y) (300, 400))

drawGame :: RSystem ()
drawGame = do
  renderer <- Apecs.get Apecs.global
  SDL.clear renderer
  drawRoom renderer (mkName "debug-room")
  drawDoor renderer (mkRectangle (100, 0) (223, 200))
  drawLevers renderer
  drawFroggits renderer
  drawNapstablook renderer
  drawFrisk renderer
  drawTextBox renderer
  SDL.present renderer
