{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components (
       RSystem
     , Time (..)
     , Frisk (..)
     , Action (..)
     , Window (..)
     , Renderer (..)
     , Resources (..)
     -- | Hide constructor.
     , Name
     -- | Export smart constructor instead.
     , mkName
     , initRuins
     , pattern Window
     , pattern Renderer
     ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import Apecs (Has (..), Storage, SystemT (..), explInit)
import qualified Apecs
import qualified Apecs.TH as Apecs
import qualified Apecs.Physics as APhysics
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup (Sum (..))
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import System.FilePath.Posix (dropExtension)
import Ruins.Apecs (makeGlobalComponent)
import Control.Monad.Reader (asks)

newtype Time = MkTime Double
  deriving (Semigroup, Monoid) via Sum Double

data Frisk = Frisk

instance Apecs.Component Frisk where
  type Storage Frisk = Apecs.Unique Frisk

data Action = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight

newtype HealthPoints = MkHealthPoints Double
  deriving newtype Num

-- | Filename without file extension.
-- | Used as a key for ResourceMap.
newtype Name = MkName Text
  deriving stock Eq
  deriving newtype Hashable

-- | Smart constructor for names
-- , making sure that the extension is dropped.
mkName :: FilePath -> Name
mkName = MkName . Text.pack . dropExtension

type ResourceMap resource = HashMap Name resource

data Resources = MkResources {
     sprites :: ResourceMap ()
   , fonts :: ResourceMap Font.Font
   , sounds :: ResourceMap Mixer.Chunk
   , music :: ResourceMap Mixer.Music
}

instance Semigroup Resources where
  MkResources {..} <> MkResources sp f s m =
    MkResources
      (sprites <> sp)
      (fonts <> f)
      (sounds <> s)
      (music <> m)

instance Monoid Resources where
  mempty =
    MkResources HMap.empty HMap.empty HMap.empty HMap.empty

-- | Since there is no way (at least I don't see any)
-- | to define a Monoid instance for SDL.Window/SDL.Renderer
-- | we need to wrap it in Maybe.
newtype Window = MkWindow (Maybe SDL.Window)
instance Semigroup Window where _previous <> next = next
instance Monoid Window where mempty = MkWindow Nothing

{-# Complete Window #-}
-- | Since window and renderer are kind of always present
-- , we don't need to match over the maybe all the time
-- , surmising that it will always be (Just window/renderer).
-- | If one does feel that the entire 'Maybe' thing is thusly superfluous
-- , one surely is right! Well, almost. Its here solely for the sake of apecs.
pattern Window :: SDL.Window -> Window
pattern Window window <- MkWindow (Just window)

newtype Renderer = MkRenderer (Maybe SDL.Renderer)
instance Semigroup Renderer where _previous <> next = next
instance Monoid Renderer where mempty = MkRenderer Nothing

{-# Complete Renderer #-}
pattern Renderer :: SDL.Renderer -> Renderer
pattern Renderer renderer <- MkRenderer (Just renderer)

traverse makeGlobalComponent [
  ''Time
  , ''Window
  , ''Renderer
  , ''Resources
  ]

Apecs.makeMapComponents [
  ''Action
  , ''HealthPoints
  ]

Apecs.makeWorld "Ruins" [
  ''Time
  , ''Frisk
  , ''Action
  , ''Window
  , ''Renderer
  , ''HealthPoints
  , ''APhysics.Physics
  ]

type RSystem result = Apecs.System Ruins result
