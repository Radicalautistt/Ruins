{-# Options -fno-warn-unused-top-binds #-}
{-# Language FlexibleContexts #-}
{-# Language ViewPatterns #-}
{-# Language NamedFieldPuns #-}

module Ruins.Draw (drawGame) where

import qualified SDL
import qualified SDL.Font as Font
import qualified Apecs
import qualified Linear
import qualified Data.Vector as Vector
import Data.Bool (bool)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Ruins.Extra.SDL (mkRectangle, rectPosition, rectExtent, Rect)
import Ruins.Extra.Apecs (pattern RXY)
import Control.Lens (ix, (^.), (&~), (-=), (+=))
import Control.Monad (when, unless)
import Unsafe.Coerce (unsafeCoerce)
import Ruins.Resources (getResource)
import Ruins.Miscellaneous (Name)
import Ruins.Components.Characters (Frisk (..), Action (..), InFight (..), Froggit (..), Napstablook (..))
import Ruins.Components.World (RSystem, sprites, Lever (..), Pressed (..), TextBox (..), Camera (..), fonts)
import Ruins.Components.Sprites (SpriteSheet (..), Sprite (..),
                                 CurrentRoomTexture (..), animationClips, currentClipIndex)

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
  let clips = _animations ^. ix rowIndex . animationClips
      currentClip = _animations Vector.! rowIndex ^. currentClipIndex
      defaultRectangle = clips Vector.! 0
      maybeRectangle = clips Vector.!? currentClip
  if _animated
     then maybe (pure defaultRectangle) pure maybeRectangle
          else pure defaultRectangle

drawPart :: SDL.Renderer -> Name -> Rect -> Rect -> RSystem ()
drawPart renderer spriteSheetName sourceRect targetRect = do
  MkSpriteSheet {_spriteSheet} <- getResource sprites spriteSheetName
  SDL.copy renderer _spriteSheet (Just sourceRect) (Just targetRect)

drawLogo :: SDL.Renderer -> RSystem ()
drawLogo renderer = do
  MkSpriteSheet {_spriteSheet} <- getResource sprites "logo"
  SDL.copy renderer _spriteSheet Nothing
    (Just do mkRectangle (380, 30) (600, 120))

drawLevers :: SDL.Renderer -> RSystem ()
drawLevers renderer = do
  Apecs.cmapM_ \ (Lever, MkPressed pressed, RXY x y, MkSprite (name, rect)) -> do
    let sourceX = bool 237 250 pressed
    drawPart renderer name rect
      (mkRectangle (x - 5, y - 25) (30, 20))
    drawPart renderer "ruins-tiles" (mkRectangle (sourceX, 138) (7, 15))
      (mkRectangle (x, y) (20, 41))

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
    dialogueFont <- getResource fonts "dialogue"
    tempSurface <- Font.solid dialogueFont white (Text.take _visibleChunk _currentText)
    textTexture <- SDL.createTextureFromSurface renderer tempSurface
    SDL.freeSurface tempSurface
    let textWidth = unsafeCoerce (_visibleChunk * 13)
    SDL.copy renderer textTexture Nothing (Just do mkRectangle (370, 520) (textWidth, 50))
    SDL.destroyTexture textTexture

drawFrisk :: SDL.Renderer -> RSystem ()
drawFrisk renderer = Apecs.cmapM_ \ (Frisk, RXY x y, action, MkInFight inFight) -> do
  if inFight
     then drawPart renderer "frisk"
            (mkRectangle (99, 0) (16, 16))
            (mkRectangle (x, y) (20, 20))
          else do let friskRow = spriteSheetRow "frisk"
                  moveUp <- friskRow 0
                  moveDown <- friskRow 1
                  moveLeft <- friskRow 2
                  moveRight <- friskRow 3

                  let targetRect = mkRectangle (x, y) friskExtent
                      draw rect = drawPart renderer "frisk" rect targetRect

                  case action of
                    MoveUp -> draw moveUp
                    MoveDown -> draw moveDown
                    MoveLeft -> draw moveLeft
                    MoveRight -> draw moveRight

drawFroggits :: SDL.Renderer -> RSystem ()
drawFroggits renderer = do
  sourceRect <- spriteSheetRow "froggit" 0
  Apecs.cmapM_ \ (Froggit, RXY x y) ->
    drawPart renderer "froggit"
      sourceRect
      (mkRectangle (x, y) (59, 60))

drawNapstablook :: SDL.Renderer -> RSystem ()
drawNapstablook renderer =
  Apecs.cmapM_ \ (Napstablook, RXY x y, MkInFight inFight) ->
    if not inFight
       then drawPart renderer "napstablook"
              (mkRectangle (15, 121) (33, 17))
              (mkRectangle (x, y) (99, 51))
            else do stareRect <- spriteSheetRow "napstablook" 0
                    drawPart renderer "napstablook"
                      stareRect (mkRectangle (x, y) (300, 400))

drawGame :: RSystem ()
drawGame = do
  renderer <- Apecs.get Apecs.global
  MkCamera {..} <- Apecs.get Apecs.global
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= black
  MkCurrentRoomTexture t <- Apecs.get Apecs.global
  SDL.copy renderer t
    (Just do mkRectangle (round $ _cameraOffset ^. Linear._x, 0) (800, 600))
    (Just do mkRectangle (0, 200) (1500, 1300))

  drawLevers renderer
  drawFroggits renderer
  drawFrisk renderer
  drawTextBox renderer
  SDL.present renderer
