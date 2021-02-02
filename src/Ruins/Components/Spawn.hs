{-# Language FlexibleContexts #-}

module Ruins.Components.Spawn where

import qualified Apecs
import qualified Apecs.Physics as Physics
import Data.Proxy (Proxy (..))
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Components.Characters as Characters
import qualified Ruins.Components.World as World

spawnFrisk :: Physics.Position -> Bool -> Characters.Action -> World.RSystem ()
spawnFrisk initialPosition inFight action =
  EApecs.newEntity_ (
    Characters.Frisk
    , action
    , Characters.HP 20
    , Characters.Speed 300
    , Characters.InFight inFight
    , Physics.KinematicBody
    , initialPosition
    , EApecs.unitVelocity
    )

killFrisk :: World.RSystem ()
killFrisk = Apecs.cmapM_ \ (Characters.Frisk, friskEntity) ->
  Apecs.destroy friskEntity $ Proxy @(
    Characters.Frisk
    , Characters.Action
    , Characters.Speed
    , Characters.InFight
    )

spawnFroggit :: Physics.Position -> World.RSystem ()
spawnFroggit position =
  EApecs.newEntity_ (
    Characters.Froggit
    , Physics.StaticBody
    , position
    )

killFroggits :: World.RSystem ()
killFroggits = Apecs.cmapM_ \ (Characters.Froggit, froggitEntity) ->
  Apecs.destroy froggitEntity (Proxy @Characters.Froggit)

spawnLever :: Physics.Position -> World.RSystem ()
spawnLever position =
  EApecs.newEntity_ (
    World.Lever
    , World.Pressed False
    , Physics.StaticBody
    , position
    )

destroyLever :: World.RSystem ()
destroyLever = Apecs.cmapM_ \ (World.Lever, leverEntity) ->
  Apecs.destroy leverEntity $ Proxy @(
    World.Lever
    , World.Pressed
    )
