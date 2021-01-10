{-# Language TemplateHaskell #-}

module Ruins.Components.Characters where

import qualified Apecs
import qualified Apecs.TH as Apecs

-- | Main hero/villain.
data Frisk = Frisk

instance Apecs.Component Frisk where
  type Storage Frisk = Apecs.Unique Frisk

-- | First boss. A really nice guy, actually.
data Napstablook = Napstablook

instance Apecs.Component Napstablook where
  type Storage Napstablook = Apecs.Unique Napstablook

-- | The flag component, that indicates
-- | how to render, etc, an associated entity.
-- | For example, in normal game Frisk are rendered as
-- | a humanoid character, while in fight scenes they are
-- | rendered as a heart.
newtype InFight = InFight Bool

-- | One of the npc's/enemies
-- | that Frisk would encounter in Ruins.
data Froggit = Froggit

newtype Speed = Speed Double
  deriving newtype Num

data Action = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight deriving stock Show

newtype HealthPoints = HealthPoints Double
  deriving newtype Num

Apecs.makeMapComponents [
  ''Action
  , ''HealthPoints
  , ''Speed
  , ''InFight
  , ''Froggit
  ]
