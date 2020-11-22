{-# Options -fno-warn-unused-top-binds #-}

module Ruins.Draw (drawGame) where

import qualified SDL
import qualified Apecs
import qualified Linear
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Ruins.SDL (mkRectangle)
import Ruins.Apecs (pattern RXY)
import Ruins.Components (RSystem, Frisk (..), Action (..), SpriteSheet (..), sprites, mkName, pattern Renderer)
import Ruins.Resources (getResource)

type Colour = Linear.V4 Word8

friskExtent :: (CInt, CInt)
friskExtent = (60, 92)

black :: Colour
black = Linear.V4 0 0 0 0

white :: Colour
white = Linear.V4 255 255 255 255

pink :: Colour
pink = Linear.V4 255 204 229 0

drawFrisk :: RSystem ()
drawFrisk = do
  Renderer renderer <- Apecs.get Apecs.global
  MkSpriteSheet {..} <- getResource sprites (mkName "frisk")
  Apecs.cmapM_ \ (Frisk, RXY x y, action :: Action) -> do
    case action of
      MoveUp -> SDL.copy renderer _spriteSheet
                  (Just do mkRectangle (0, 0) (19, 29)) (Just (mkRectangle (x, y) friskExtent))

      _ -> pure ()

drawGame :: RSystem ()
drawGame = do
  Renderer renderer <- Apecs.get Apecs.global
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= pink
  drawFrisk
  SDL.present renderer
