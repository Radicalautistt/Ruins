{-# Options -fno-warn-unused-top-binds #-}
{-# Language FlexibleContexts #-}
{-# Language ViewPatterns #-}

module Ruins.Draw (drawGame) where

import qualified SDL
import qualified SDL.Font as Font
import qualified Apecs
import qualified Linear
import qualified Data.Vector as Vector
import GHC.Int (Int32 (..))
import Data.Bool (bool)
import qualified Data.Text as Text
import Data.Word (Word8)
import Data.HashMap.Strict (HashMap)
import qualified Data.Array.IArray as Array
import qualified Data.HashMap.Strict as HMap
import Foreign.C.Types (CInt (..))
import Ruins.SDL (mkRectangle, rectPosition, rectExtent, Rect)
import Ruins.Apecs (pattern RXY)
import Data.Foldable (for_)
import Control.Lens (ifor_, view, ix, (^.), (&~), (-=), (+=))
import Control.Monad (when, unless)
import Unsafe.Coerce (unsafeCoerce)
import Ruins.Resources (getResource)
import Ruins.Miscellaneous (Name, mkName)
import Ruins.Components.Characters (Frisk (..), Action (..), InFight (..), Froggit (..), Napstablook (..))
import Ruins.Components.World (RSystem, sprites, NewRoom (..),
                               Lever (..), Pressed (..), TextBox (..), fonts)

import Ruins.Components.Sprites (SpriteSheet (..), NewTileMap (..), Sprite (..),
                                 animationClips, currentClipIndex, spriteSheet)

type Colour = Linear.V4 Word8

friskExtent :: (CInt, CInt)
friskExtent = (60, 92)

black :: Colour
black = Linear.V4 0 0 0 0

white :: Colour
white = Linear.V4 255 255 255 255

pink :: Colour
pink = Linear.V4 255 204 229 0

tileRects :: HashMap Int32 Rect
tileRects = HMap.fromList ([0..72] `zip` rects)
  where block = (20, 20)
        mkBlock = flip mkRectangle block
        rects = [
            mkBlock (0, 0)
          , mkBlock (20, 0)
          , mkBlock (40, 0)
          , mkBlock (60, 0)
          , mkBlock (80, 0)
          , mkBlock (100, 0)
          , mkBlock (120, 0)
          , mkBlock (0, 20)
          , mkBlock (20, 20)
          , mkBlock (40, 20)
          , mkBlock (60, 20)
          , mkBlock (80, 20)
          , mkBlock (100, 20)
          , mkBlock (120, 20)
          , mkBlock (0, 40)
          , mkBlock (20, 40)
          , mkBlock (40, 40)
          , mkBlock (60, 40)
          , mkBlock (80, 40)
          , mkBlock (100, 40)
          , mkBlock (120, 40)
          , mkBlock (0,  60)
          , mkBlock (20, 60)
          , mkBlock (40, 60)
          , mkBlock (60, 60)
          , mkBlock (80, 60)
          , mkBlock (100, 60)
          , mkBlock (120, 60)

          , mkRectangle (144, 0) (60, 20)
          , mkRectangle (144, 22) (40, 20)
          , mkRectangle (144, 44) (40, 20)
          , mkRectangle (189, 25) (38, 38)
          , mkRectangle (0, 89) (80, 40)
          , mkRectangle (86, 89) (40, 40)
          , mkRectangle (144, 67) (40, 58)
          , mkRectangle (188, 67) (60, 60)
          , mkRectangle (255, 15) (99, 112)
          , mkRectangle (0, 138) (60, 40)
          , mkRectangle (66, 138) (40, 40)
          , mkRectangle (112, 138) (28, 28)
          , mkRectangle (145, 138) (16, 20)
          , mkRectangle (165, 138) (20, 20)
          , mkRectangle (191, 139) (16, 14)
          , mkRectangle (215, 141) (16, 12)
          , mkRectangle (237, 138) (7, 15)
          , mkRectangle (250, 138) (7, 15)
          , mkRectangle (251, 138) (14, 16)
          , mkRectangle (279, 138) (14, 16)
          , mkRectangle (297, 138) (14, 16)
          , mkRectangle (112, 170) (14, 13)
          , mkRectangle (128, 170) (14, 12)
          , mkRectangle (145, 162) (64, 20)
          , mkRectangle (214, 163) (18, 18)
          , mkRectangle (238, 163) (18, 18)
          , mkRectangle (261, 159) (18, 23)
          , mkRectangle (282, 155) (20, 27)
          , mkRectangle (305, 162) (29, 20)
          , mkRectangle (337, 175) (26, 7)
          , mkRectangle (0, 187) (60, 40)
          , mkRectangle (66, 187) (40, 40)
          , mkRectangle (106, 187) (40, 40)
          , mkRectangle (151, 187) (40, 40)
          , mkRectangle (213, 187) (20, 19)
          , mkRectangle (237, 187) (34, 32)
          , mkRectangle (274, 187) (18, 16)
          , mkRectangle (294, 187) (19, 20)
          , mkRectangle (315, 187) (28, 15)
          , mkRectangle (0, 235) (60, 40)
          , mkRectangle (66, 227) (40, 40)
          , mkRectangle (106, 227) (40, 40)
          , mkRectangle (213, 209) (20, 18)
          , mkRectangle (274, 209) (36, 60)
          , mkRectangle (313, 209) (34, 56)
                ]

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
   
-- drawRoom :: SDL.Renderer -> Name -> RSystem ()
-- drawRoom renderer roomName = do
--   MkRooms rooms <- Apecs.get Apecs.global
--   let maybeRoom = HMap.lookup roomName rooms
--       errorMessage = Text.unpack ("drawRoom: no such room " <> getName roomName)
--   maybe (fail errorMessage) draw maybeRoom
--   drawLogo renderer
--   where draw MkRoom {..} = do
--           let Right MkTileMap {..} = _background
--           sourceSheet <- view spriteSheet <$> getResource sprites _sourceName
--           ifor_ _tileMap \ rowIndex row ->
--             ifor_ row \ columnIndex column ->
--               SDL.copy renderer sourceSheet (Just (column ^. tileRectangle))
--                 (Just (mkRectangle (unsafeCoerce (columnIndex * 20 * 2, rowIndex * 20 * 2)) (20 * 2,20 * 2)))

drawRoom :: SDL.Renderer -> RSystem ()
drawRoom renderer = do
  MkNewRoom {..} <- Apecs.get Apecs.global
  either drawSprite drawTileMap _roomBackground
  where drawSprite _ = pure ()
        drawTileMap MkNewTileMap {..} = do
          sourceSheet <- view spriteSheet <$> getResource sprites _newSourceName
          case Array.bounds _newTileMap of
            _ ->
              for_ [0..11] \ rowIndex ->
                for_ [0..15] \ columnIndex ->
                  case _newTileMap Array.! (rowIndex * 16 + columnIndex) of
                    tile -> case HMap.lookup tile tileRects of
                      Nothing -> pure ()
                      rect -> SDL.copy renderer sourceSheet rect
                        (Just $ mkRectangle (CInt (columnIndex * 20), CInt (rowIndex * 20)) (20, 20))

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
  SDL.rendererDrawColor renderer SDL.$= pink
  drawRoom renderer
  drawDoor renderer (mkRectangle (100, 0) (223, 200))
  drawLevers renderer
  drawFroggits renderer
  drawNapstablook renderer
  drawFrisk renderer
  drawTextBox renderer
  SDL.present renderer
