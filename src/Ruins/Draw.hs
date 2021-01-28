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
import Foreign.C.Types (CInt (..))
import Control.Lens (ix, (^.), (&~), (-=), (+=))
import Control.Monad (when, unless)
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

drawTextBox :: SDL.Renderer -> World.RSystem ()
drawTextBox renderer = do
  World.TextBox {..} <- Apecs.get Apecs.global
  when _opened do
    drawRectangle renderer (ESDL.mkRectangle (350, 500) (700, 200)) black white
    dialogueFont <- Resources.getResource World.fonts "dialogue"
    tempSurface <- Font.solid dialogueFont white (Text.take _visibleChunk _currentText)
    textTexture <- SDL.createTextureFromSurface renderer tempSurface
    SDL.freeSurface tempSurface
    let textWidth = fromIntegral (_visibleChunk * 13)
    SDL.copy renderer textTexture Nothing (Just do ESDL.mkRectangle (370, 520) (textWidth, 50))
    SDL.destroyTexture textTexture

drawFrisk :: SDL.Renderer -> World.RSystem ()
drawFrisk renderer = Apecs.cmapM_ \ (Characters.Frisk, EApecs.RXY x y, action, Characters.InFight inFight) -> do
  if inFight
     then drawPart renderer "frisk"
            (ESDL.mkRectangle (99, 0) (16, 16))
            (ESDL.mkRectangle (x, y) (20, 20))
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

drawGame :: World.RSystem ()
drawGame = do
  renderer <- Apecs.get Apecs.global
  World.Camera {..} <- Apecs.get Apecs.global
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= black
  Sprites.Background {..} <- Apecs.get Apecs.global
  SDL.copy renderer _backgroundTexture
    (Just do ESDL.mkRectangle (round $ _cameraOffset ^. Linear._x, 0) (800, 600))
    (Just _backgroundRectangle)

  drawLevers renderer
  drawFroggits renderer
  drawFrisk renderer
  drawTextBox renderer
  SDL.present renderer
