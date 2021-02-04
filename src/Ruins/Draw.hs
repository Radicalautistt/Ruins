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
import Data.Text (Text)
import Data.Bool (bool)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Foreign.C.Types (CInt (..))
import Data.Foldable (for_)
import Control.Lens (ix, (^.), (&~), (-=), (+=))
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef
import qualified Ruins.Resources as Resources
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Extra.SDL as ESDL
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

friskExtent :: (CInt, CInt)
friskExtent = (60, 92)

black :: ESDL.Color
black = Linear.V4 0 0 0 0

white :: ESDL.Color
white = Linear.V4 255 255 255 0

pink :: ESDL.Color
pink = Linear.V4 255 204 229 0

spriteSheetRow :: Misc.Name -> Int -> World.RSystem ESDL.Rect
spriteSheetRow spriteSheetName rowIndex = do
  Sprites.SpriteSheet {..} <- Resources.getResource World.sprites spriteSheetName
  let clips = _animations ^. ix rowIndex . Sprites.animationClips
      currentClip = _animations Vector.! rowIndex ^. Sprites.currentClipIndex
      defaultRectangle = clips Vector.! 0
      maybeRectangle = clips Vector.!? currentClip
  if _animated
     then maybe (pure defaultRectangle) pure maybeRectangle
          else pure defaultRectangle

drawPart :: SDL.Renderer -> Misc.Name -> ESDL.Rect -> ESDL.Rect -> World.RSystem ()
drawPart renderer spriteSheetName sourceRect targetRect = do
  Sprites.SpriteSheet {_spriteSheet} <- Resources.getResource World.sprites spriteSheetName
  SDL.copy renderer _spriteSheet (Just sourceRect) (Just targetRect)

drawLogo :: SDL.Renderer -> World.RSystem ()
drawLogo renderer = do
  Sprites.SpriteSheet {_spriteSheet} <- Resources.getResource World.sprites "logo"
  SDL.copy renderer _spriteSheet Nothing
    (Just do ESDL.mkRectangle (380, 30) (600, 120))

drawLevers :: SDL.Renderer -> World.RSystem ()
drawLevers renderer =
  Apecs.cmapM_ \ (World.Lever, World.Pressed pressed, EApecs.RXY x y) -> do
    let sourceX = bool 237 250 pressed
    drawPart renderer "ruins-tiles" (ESDL.mkRectangle (sourceX, 138) (7, 15))
      (ESDL.mkRectangle (x, y) (20, 41))

drawRectangle :: SDL.Renderer -> ESDL.Rect -> ESDL.Color -> ESDL.Color -> World.RSystem ()
drawRectangle renderer rectangle foreground background = do
  SDL.rendererDrawColor renderer SDL.$= background
  renderer `SDL.fillRect` Just rectangle
  let innerRectangle = rectangle &~ do
        ESDL.rectExtent Linear._x -= 20
        ESDL.rectExtent Linear._y -= 20

        ESDL.rectPosition Linear._x += 10
        ESDL.rectPosition Linear._y += 10

  unless (foreground == background) do
    SDL.rendererDrawColor renderer SDL.$= foreground
    renderer `SDL.fillRect` Just innerRectangle

drawText :: SDL.Renderer -> Text -> Misc.Name -> ESDL.Rect -> World.RSystem ()
drawText renderer text fontName targetRectangle = do
  font <- Resources.getResource World.fonts fontName
  tempSurface <- Font.solid font white text
  texture <- SDL.createTextureFromSurface renderer tempSurface
  SDL.freeSurface tempSurface
  SDL.copy renderer texture Nothing (Just targetRectangle)
  SDL.destroyTexture texture

drawTextBox :: SDL.Renderer -> World.RSystem ()
drawTextBox renderer = do
  World.TextBox {..} <- Apecs.get Apecs.global
  when _textBoxOpened do
    case fromIntegral (_textBoxVisibleChunk * 20) of
      textWidth -> case _textBoxPosition of
        World.Top -> do
          drawRectangle renderer (ESDL.mkRectangle (340, 10) (700, 200)) black white
          drawText renderer (Text.take _textBoxVisibleChunk _textBoxCurrentText)
            "dialogue" (ESDL.mkRectangle (370, 30) (textWidth, 50))

        World.Bottom -> do
          drawRectangle renderer (ESDL.mkRectangle (340, 500) (700, 200)) black white
          drawText renderer (Text.take _textBoxVisibleChunk _textBoxCurrentText)
            "dialogue" (ESDL.mkRectangle (370, 520) (textWidth, 50))

drawItems :: SDL.Renderer -> World.RSystem ()
drawItems renderer = Apecs.cmapM_ \ (item, EApecs.RXY x y) ->
  case item of
    Characters.ToyKnife ->
      drawItem (ESDL.mkRectangle (174, 230) (14, 13)) (ESDL.mkRectangle (x, y) (50, 42))

    Characters.FadedRibbon ->
      drawItem (ESDL.mkRectangle (190, 230) (14, 12)) (ESDL.mkRectangle (x, y) (50, 42))

    Characters.ButterscotchPie ->
      drawItem (ESDL.mkRectangle (152, 230) (20, 14)) (ESDL.mkRectangle (x, y) (60, 42))

    Characters.MonsterCandy -> pure ()

  where drawItem source target =
          drawPart renderer "ruins-tiles" source target

drawHeart :: SDL.Renderer -> ESDL.Rect -> World.RSystem ()
drawHeart renderer targetRectangle =
  drawPart renderer "frisk"
    (ESDL.mkRectangle (99, 0) (16, 16)) targetRectangle

drawFrisk :: SDL.Renderer -> World.RSystem ()
drawFrisk renderer = Apecs.cmapM_ \ (Characters.Frisk, EApecs.RXY x y, action, Characters.InFight inFight) -> do
  if inFight
     then drawHeart renderer (ESDL.mkRectangle (x, y) (20, 20))
          else do let friskRow = spriteSheetRow "frisk"
                  moveUp <- friskRow 0
                  moveDown <- friskRow 1
                  moveLeft <- friskRow 2
                  moveRight <- friskRow 3

                  let targetRect = ESDL.mkRectangle (x, y) friskExtent
                      draw rect = drawPart renderer "frisk" rect targetRect

                  case action of
                    Characters.MoveUp -> draw moveUp
                    Characters.MoveDown -> draw moveDown
                    Characters.MoveLeft -> draw moveLeft
                    Characters.MoveRight -> draw moveRight

drawFroggits :: SDL.Renderer -> World.RSystem ()
drawFroggits renderer = do
  sourceRect <- spriteSheetRow "froggit" 0
  Apecs.cmapM_ \ (Characters.Froggit, EApecs.RXY x y) ->
    drawPart renderer "froggit"
      sourceRect
      (ESDL.mkRectangle (x, y) (59, 60))

drawNapstablook :: SDL.Renderer -> World.RSystem ()
drawNapstablook renderer =
  Apecs.cmapM_ \ (Characters.Napstablook, EApecs.RXY x y, Characters.InFight inFight) ->
    if not inFight
       then drawPart renderer "napstablook"
              (ESDL.mkRectangle (15, 121) (33, 17))
              (ESDL.mkRectangle (x, y) (99, 51))
            else do stareRect <- spriteSheetRow "napstablook" 0
                    drawPart renderer "napstablook"
                      stareRect (ESDL.mkRectangle (x, y) (300, 400))

drawMenu :: SDL.Renderer -> World.RSystem ()
drawMenu renderer = Apecs.cmapM_ \ (Characters.Frisk, Characters.HP hp) -> do
  World.Menu {..} <- Apecs.get Apecs.global
  when _menuOpened do
    drawMenuChunk (ESDL.mkRectangle (50, 100) (230, 168))
    drawMenuText "N." (ESDL.mkRectangle (80, 110) (50, 50))
    drawMenuText "LV 1" (ESDL.mkRectangle (80, 160) (50, 25))
    drawMenuText ("HP " <> Text.pack (show hp) <> "/20") (ESDL.mkRectangle (80, 190) (100, 25))
    drawMenuText "G  0" (ESDL.mkRectangle (80, 220) (50, 25))

    drawMenuChunk (ESDL.mkRectangle (50, 280) (230, 230))
    drawMenuText "ITEM" (ESDL.mkRectangle (140, 310) (85, 50))
    drawMenuText "STAT" (ESDL.mkRectangle (140, 370) (85, 50))
    drawMenuText "CELL" (ESDL.mkRectangle (140, 430) (85, 50))
    case _menuState of
      World.ItemMenu indexOrAction -> do
        drawMenuChunk (ESDL.mkRectangle (305, 100) (530, 500))
        drawMenuText "USE        INFO        DROP" (ESDL.mkRectangle (378, 510) (405, 50))

        World.Inventory inventory <- Apecs.get Apecs.global
        currentYPosition <- liftIO (newIORef @CInt 130)
        for_ inventory \ item -> do
          yPosition <- liftIO (readIORef currentYPosition)
          drawMenuText (Text.pack (show item)) (ESDL.mkRectangle (380, yPosition) (fromIntegral $ length (show item) * 20, 50))
          liftIO (modifyIORef' currentYPosition (+ 50))

        case indexOrAction of
          Left _index -> pure ()
          Right itemAction -> case itemAction of
            World.Use -> drawHeart renderer (ESDL.mkRectangle (338, 520) (30, 30))
            World.Info -> drawHeart renderer (ESDL.mkRectangle (485, 520) (30, 30))
            World.Drop -> drawHeart renderer (ESDL.mkRectangle (650, 520) (30, 30))

      World.DefaultMenu menuFocus ->
        case menuFocus of
          World.ItemFocus -> drawHeart renderer (ESDL.mkRectangle (100, 320) (30, 30))
          World.StatFocus -> drawHeart renderer (ESDL.mkRectangle (100, 380) (30, 30))
          World.CellFocus -> drawHeart renderer (ESDL.mkRectangle (100, 440) (30, 30))

      World.StatMenu -> do
        drawMenuChunk (ESDL.mkRectangle (305, 100) (530, 600))
        drawMenuText "\"N.\"" (ESDL.mkRectangle (350, 130) (80, 60))
        drawMenuText "LV 1" (ESDL.mkRectangle (350, 220) (85, 50))
        drawMenuText ("HP " <> Text.pack (show hp) <> "/20") (ESDL.mkRectangle (350, 260) (180, 50))
        drawMenuText "AT 0(0)          EXP: 0" (ESDL.mkRectangle (350, 350) (365, 50))
        drawMenuText "DF 0(0)          NEXT: 10" (ESDL.mkRectangle (350, 390) (412, 50))
        drawMenuText "WEAPON: Stick" (ESDL.mkRectangle (350, 480) (280, 50))
        drawMenuText "ARMOR: Bandage" (ESDL.mkRectangle (350, 530) (315, 50))
        drawMenuText "GOLD: 0" (ESDL.mkRectangle (350, 600) (140, 50))

      World.CellMenu cellAction -> do
        drawMenuChunk (ESDL.mkRectangle (305, 100) (530, 420))
        drawMenuText "Say Hello" (ESDL.mkRectangle (380, 130) (200, 50))
        drawMenuText "About Yourself" (ESDL.mkRectangle (380, 180) (300, 50))
        drawMenuText "Call Her \"Mom\"" (ESDL.mkRectangle (380, 230) (290, 50))
        drawMenuText "Flirt" (ESDL.mkRectangle (380, 280) (105, 50))
        drawMenuText "Puzzle Help" (ESDL.mkRectangle (380, 330) (250, 50))

        case cellAction of
          World.Flirt -> drawHeart renderer (ESDL.mkRectangle (340, 290) (30, 30))
          World.SayHello -> drawHeart renderer (ESDL.mkRectangle (340, 140) (30, 30))
          World.PuzzleHelp -> drawHeart renderer (ESDL.mkRectangle (340, 340) (30, 30))
          World.CallHerMom -> drawHeart renderer (ESDL.mkRectangle (340, 240) (30, 30))
          World.AboutYourself -> drawHeart renderer (ESDL.mkRectangle (340, 190) (30, 30))

  where drawMenuChunk rectangle =
          drawRectangle renderer rectangle black white
        drawMenuText text rectangle =
          drawText renderer text "dialogue" rectangle

drawBackground :: SDL.Renderer -> World.RSystem ()
drawBackground renderer = do
  (World.Camera {..},
   Sprites.Background {..}) <- Apecs.get Apecs.global

  case round (_cameraOffset ^. Linear._x) of
    xoffset
      | not _cameraActive ->
          SDL.copy renderer
            _backgroundTexture Nothing (Just _backgroundRectangle)

      | otherwise ->
          SDL.copy renderer
            _backgroundTexture
            (Just do ESDL.mkRectangle (xoffset, 0) (fromJust _cameraViewport))
            (Just _backgroundRectangle)

drawGame :: World.RSystem ()
drawGame = do
  renderer <- Apecs.get Apecs.global
  SDL.clear renderer

  SDL.rendererDrawColor renderer SDL.$= black
  drawBackground renderer
  drawLevers renderer
  drawFroggits renderer
  drawItems renderer
  drawFrisk renderer
  drawTextBox renderer
  drawMenu renderer

  SDL.present renderer
