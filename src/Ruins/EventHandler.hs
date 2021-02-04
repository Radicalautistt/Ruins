{-# Language MultiWayIf #-}
{-# Language RankNTypes #-}
{-# Language ViewPatterns #-}
{-# Language NamedFieldPuns #-}
{-# Language FlexibleContexts #-}

module Ruins.EventHandler (
       handleEvents
     , handleKeyboardState
     , animateIndefinitely
     ) where

import qualified SDL
import qualified Apecs
import qualified Apecs.Physics as Physics
import qualified Linear
import qualified Data.Text as Text
import Data.Foldable (for_)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HMap
import Control.Lens (Lens', set, over, (&), (&~), (.=), (%=), (.~), (+~), (-~))
import Control.Monad (when, unless)
import qualified Ruins.Keys as Keys
import qualified Ruins.Audio as Audio
import qualified Ruins.Script as Script
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.Spawn as Spawn
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Sprites as Sprites
import qualified Ruins.Components.Characters as Characters

defaultSpeed :: Characters.Speed
defaultSpeed = Characters.Speed 400

givePositiveVelocity :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> World.RSystem ()
givePositiveVelocity entity axisLens =
  Apecs.modify entity \ (EApecs.VEL _ _, Characters.Speed speed) ->
    EApecs.unitVelocity & EApecs.velocityVector . axisLens +~ speed

giveNegativeVelocity :: Apecs.Entity -> Lens' (Linear.V2 Double) Double -> World.RSystem ()
giveNegativeVelocity entity axisLens =
  Apecs.modify entity \ (EApecs.VEL _ _, Characters.Speed speed) ->
    EApecs.unitVelocity & EApecs.velocityVector . axisLens -~ speed

dropVelocityOf :: Apecs.Entity -> World.RSystem ()
dropVelocityOf entity = Apecs.modify entity \ (EApecs.VEL _ _) -> EApecs.unitVelocity

{-# Inline toggleAnimation #-}
toggleAnimation :: Misc.Name -> Bool -> World.RSystem ()
toggleAnimation spriteSheetName enabled =
  Apecs.modify Apecs.global $
    over World.sprites (HMap.update (Just . set Sprites.animated enabled) spriteSheetName)

animate :: Misc.Name -> World.RSystem ()
animate spriteSheetName = toggleAnimation spriteSheetName True

stopAnimation :: Misc.Name -> World.RSystem ()
stopAnimation spriteSheetName = toggleAnimation spriteSheetName False

-- | Enable animations by default for the given spritesheet names.
-- | Useful when you have some NPCs with a looping animation routine.
animateIndefinitely :: [Misc.Name] -> World.RSystem ()
animateIndefinitely names = for_ names animate

move :: Apecs.Entity -> Characters.Action -> Misc.Name -> World.RSystem ()
move entity direction animationName = do
  entity Apecs.$= direction
  setVelocityValue
  animate animationName
  where setVelocityValue =
          case direction of
            Characters.MoveUp ->
              giveNegativeVelocity entity Linear._y

            Characters.MoveDown ->
              givePositiveVelocity entity Linear._y

            Characters.MoveLeft ->
              giveNegativeVelocity entity Linear._x

            Characters.MoveRight ->
              givePositiveVelocity entity Linear._x

stop :: Apecs.Entity -> Misc.Name -> World.RSystem ()
stop entity animationName = do
  dropVelocityOf entity
  stopAnimation animationName

setMenuState :: World.MenuState -> World.RSystem ()
setMenuState newState =
  Apecs.global Apecs.$~ set World.menuState newState

toggleMenu :: World.RSystem ()
toggleMenu = do
  Audio.playSound "menu-move"
  Apecs.global Apecs.$~ \ menu -> menu &~ do
    World.menuOpened %= not
    World.menuState .= World.DefaultMenu World.ItemFocus

handleMenuCursor :: SDL.EventPayload -> World.MenuState -> World.RSystem ()
handleMenuCursor pressedKey menuState =
  case menuState of
    World.DefaultMenu menuFocus ->
      case menuFocus of
        World.ItemFocus -> case pressedKey of
          Keys.PRESSED_UP -> pure ()
          Keys.PRESSED_DOWN -> moveDefaultMenu World.StatFocus
          _otherwise -> pure ()

        World.StatFocus -> case pressedKey of
          Keys.PRESSED_UP -> moveDefaultMenu World.ItemFocus
          Keys.PRESSED_DOWN -> moveDefaultMenu World.CellFocus
          _otherwise -> pure ()

        World.CellFocus -> case pressedKey of
          Keys.PRESSED_UP -> moveDefaultMenu World.StatFocus
          Keys.PRESSED_DOWN -> pure ()
          _otherwise -> pure ()

    World.ItemMenu indexOrAction -> case indexOrAction of
      Left index -> do
        World.Inventory inventory <- Apecs.get Apecs.global
        case pressedKey of
          Keys.PRESSED_UP
            | index - 1 > 0 -> moveItemMenu (Left (index - 1))
            | otherwise -> pure ()

          Keys.PRESSED_DOWN
            | index + 1 < Vector.length inventory -> moveItemMenu (Left (index + 1))
            | otherwise -> pure ()

          _otherwise -> pure ()

      Right itemAction -> case itemAction of
        World.Use -> case pressedKey of
          Keys.PRESSED_RIGHT -> moveItemMenu (Right World.Info)
          _otherwise -> pure ()

        World.Info -> case pressedKey of
          Keys.PRESSED_LEFT -> moveItemMenu (Right World.Use)
          Keys.PRESSED_RIGHT -> moveItemMenu (Right World.Drop)
          _otherwise -> pure ()

        World.Drop -> case pressedKey of
          Keys.PRESSED_LEFT -> moveItemMenu (Right World.Info)
          _otherwise -> pure ()

    World.CellMenu cellAction -> case cellAction of
      World.SayHello -> case pressedKey of
        Keys.PRESSED_DOWN -> moveCellMenu World.AboutYourself
        _otherwise -> pure ()

      World.AboutYourself -> case pressedKey of
        Keys.PRESSED_UP -> moveCellMenu World.SayHello
        Keys.PRESSED_DOWN -> moveCellMenu World.CallHerMom
        _otherwise -> pure ()

      World.CallHerMom -> case pressedKey of
        Keys.PRESSED_UP -> moveCellMenu World.AboutYourself
        Keys.PRESSED_DOWN -> moveCellMenu World.Flirt
        _otherwise -> pure ()

      World.Flirt -> case pressedKey of
        Keys.PRESSED_UP -> moveCellMenu World.CallHerMom
        Keys.PRESSED_DOWN -> moveCellMenu World.PuzzleHelp
        _otherwise -> pure ()

      World.PuzzleHelp -> case pressedKey of
        Keys.PRESSED_UP -> moveCellMenu World.Flirt
        _otherwise -> pure ()

    _otherwise -> pure ()

    where moveDefaultMenu direction = do
            Audio.playSound "menu-move"
            setMenuState (World.DefaultMenu direction)

          moveCellMenu direction = do
            Audio.playSound "menu-move"
            setMenuState (World.CellMenu direction)

          moveItemMenu direction = do
            Audio.playSound "menu-move"
            setMenuState (World.ItemMenu direction)

handleMenuAction :: SDL.EventPayload -> World.MenuState -> World.RSystem ()
handleMenuAction pressedKey menuState =
  case menuState of
    World.DefaultMenu menuFocus -> case menuFocus of
      World.ItemFocus -> case pressedKey of
        Keys.PRESSED_X -> toggleMenu
        Keys.PRESSED_Z -> selectMenu (World.ItemMenu (Right World.Use))
        _otherwise -> pure ()

      World.StatFocus -> case pressedKey of
        Keys.PRESSED_X -> toggleMenu
        Keys.PRESSED_Z -> selectMenu World.StatMenu
        _otherwise -> pure ()

      World.CellFocus -> case pressedKey of
        Keys.PRESSED_X -> toggleMenu
        Keys.PRESSED_Z -> selectMenu (World.CellMenu World.SayHello)
        _otherwise -> pure ()

    World.ItemMenu _ -> case pressedKey of
      Keys.PRESSED_X -> setMenuState (World.DefaultMenu World.ItemFocus)
      _otherwise -> pure ()

    World.StatMenu -> case pressedKey of
      Keys.PRESSED_X -> setMenuState (World.DefaultMenu World.StatFocus)
      _otherwise -> pure ()

    World.CellMenu _ -> case pressedKey of
      Keys.PRESSED_X -> setMenuState (World.DefaultMenu World.CellFocus)
      _otherwise -> pure ()

    where selectMenu newState = do
            Audio.playSound "menu-select"
            setMenuState newState

handleKeyboardState :: World.RSystem ()
handleKeyboardState = do
  World.Menu {_menuOpened} <- Apecs.get Apecs.global
  unless _menuOpened do
    Apecs.cmapM_ \ (Characters.Frisk, friskEntity) -> do
      keyIs <- SDL.getKeyboardState
      let moveFrisk direction = move friskEntity direction "frisk"

      if | keyIs SDL.ScancodeUp -> moveFrisk Characters.MoveUp
         | keyIs SDL.ScancodeDown -> moveFrisk Characters.MoveDown
         | keyIs SDL.ScancodeLeft -> moveFrisk Characters.MoveLeft
         | keyIs SDL.ScancodeRight -> moveFrisk Characters.MoveRight
         | otherwise -> stop friskEntity "frisk"

handleInGameAction :: SDL.EventPayload -> World.RSystem ()
handleInGameAction = \ case
  Keys.PRESSED_Z ->
    Apecs.cmapM_ \ (Characters.Frisk, Physics.Position friskPosition) -> do
      Apecs.cmapM_ \ (item, Physics.Position itemPosition, itemEntity) ->
        when (Linear.norm (itemPosition - friskPosition) < 50) do
          Apecs.global Apecs.$~ \ (World.Inventory inventory) ->
            World.Inventory (inventory `Vector.snoc` item)

          Audio.playSound "item-equiped"
          Spawn.destroyItem itemEntity
          Script.sayDefault World.Top ("* You found the " <> Text.pack (show item) <> ".")

      Apecs.cmapM_ \ (World.Lever, Physics.Position leverPosition, leverEntity) -> do
        when (Linear.norm (leverPosition - friskPosition) < 30) do
          leverEntity Apecs.$~ \ (World.Pressed pressed) ->
            World.Pressed (not pressed)

          Audio.playSound "lever-toggle"
          Script.sayDefault World.Bottom "* You've successfully toggled this lever."

      World.TextBox {..} <- Apecs.get Apecs.global
      when (_textBoxOpened && not _textBoxActive) do
        Apecs.global Apecs.$~ set World.textBoxOpened False

  Keys.PRESSED_X -> do
    World.TextBox {_textBoxOpened} <- Apecs.get Apecs.global
    when _textBoxOpened do
      Apecs.global Apecs.$~ \ textBox@World.TextBox {..} ->
        textBox & World.textBoxVisibleChunk .~ Text.length _textBoxCurrentText - 1

  _otherwise -> pure ()

handleEvent :: SDL.EventPayload -> World.RSystem ()
handleEvent = \ case
  Keys.PRESSED_C -> toggleMenu
  Keys.PRESSED_S -> Audio.playSound "speedup" *> setFriskSpeed (defaultSpeed * 2)
  Keys.PRESSED_D -> Audio.playSound "stop" *> setFriskSpeed defaultSpeed
  Keys.PRESSED_H -> Audio.toggleAudio
  Keys.PRESSED_ESCAPE -> Apecs.global Apecs.$= World.QuitGame True

  Keys.JOYSTICK_UP -> moveFrisk Characters.MoveUp
  Keys.JOYSTICK_DOWN -> moveFrisk Characters.MoveDown
  Keys.JOYSTICK_LEFT -> moveFrisk Characters.MoveLeft
  Keys.JOYSTICK_RIGHT -> moveFrisk Characters.MoveRight
  Keys.JOYSTICK_CENTERED -> stopFrisk

  _otherwise -> do
    World.Menu {..} <- Apecs.get Apecs.global
    if | not _menuOpened -> handleInGameAction _otherwise
       | otherwise -> do
           stopFrisk
           handleMenuCursor _otherwise _menuState
           handleMenuAction _otherwise _menuState

  where moveFrisk direction = Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
          move friskEntity direction "frisk"

        stopFrisk = Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
          stop friskEntity "frisk"

        setFriskSpeed speed = Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
          friskEntity Apecs.$= speed

handleEvents :: World.RSystem ()
handleEvents = do
  (map SDL.eventPayload -> eventPayloads) <- SDL.pollEvents
  when (SDL.QuitEvent `elem` eventPayloads) do
    Apecs.global Apecs.$= World.QuitGame True

  for_ eventPayloads handleEvent
