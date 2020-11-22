{-# Options -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses #-}

module Ruins.Components (
       RSystem
     , Time (..)
     , Frisk (..)
     , Speed (..)
     , Action (..)
     , Window (..)
     , Renderer (..)
     , QuitGame (..)
     , Resources (..)
     , ResourceMap
     , SpriteSheet (..)
     -- | Hide constructor.
     , Name
     -- | Export smart constructor instead.
     , mkName
     , getName
     , initRuins
     , spriteSheet
     , animations
     , sprites
     , fonts
     , sounds
     , music
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
import Data.Semigroup (Sum (..), Any (..))
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import System.FilePath.Posix (dropExtension)
import Ruins.Apecs (makeGlobalComponent)
import Control.Lens (makeLenses)
import Control.Monad.Reader (asks)
import Control.Monad.Managed (Managed)

newtype Time = MkTime Double
  deriving newtype Num
  deriving (Semigroup, Monoid) via Sum Double

data Frisk = Frisk

instance Apecs.Component Frisk where
  type Storage Frisk = Apecs.Unique Frisk

newtype Speed = MkSpeed Double
  deriving newtype Num

data Action = MoveUp
   | MoveDown
   | MoveLeft
   | MoveRight

newtype HealthPoints = MkHealthPoints Double
  deriving newtype Num

-- | Filename without file extension.
-- | Used as a key for ResourceMap.
newtype Name = MkName { getName :: Text }
  deriving stock Eq
  deriving newtype Hashable

-- | Smart constructor for names
-- , making sure that the extension is dropped.
mkName :: FilePath -> Name
mkName = MkName . Text.pack . dropExtension

data Animation

data SpriteSheet = MkSpriteSheet {
     _spriteSheet :: SDL.Texture
   , _animations :: HashMap Name Animation
}

type ResourceMap resource = HashMap Name resource

data Resources = MkResources {
     _sprites :: ResourceMap SpriteSheet
   , _fonts :: ResourceMap Font.Font
   , _sounds :: ResourceMap Mixer.Chunk
   , _music :: ResourceMap Mixer.Music
}

instance Semigroup Resources where
  MkResources {..} <> MkResources sp f s m =
    MkResources
      (_sprites <> sp)
      (_fonts <> f)
      (_sounds <> s)
      (_music <> m)

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

-- | Global flag telling the game whether it should end or not.
newtype QuitGame = MkQuitGame Bool
  deriving (Semigroup, Monoid) via Any

traverse makeGlobalComponent [
  ''Time
  , ''Window
  , ''Renderer
  , ''QuitGame
  , ''Resources
  ]

Apecs.makeMapComponents [
  ''Action
  , ''Speed
  , ''HealthPoints
  ]

Apecs.makeWorld "Ruins" [
  ''Time
  , ''Frisk
  , ''Speed
  , ''Action
  , ''Window
  , ''Renderer
  , ''QuitGame
  , ''Resources
  , ''HealthPoints
  , ''APhysics.Physics
  ]

concat <$> traverse makeLenses [
  ''SpriteSheet
  , ''Resources
  ]

type RSystem result = Apecs.SystemT Ruins Managed result

-- | We need this to actually use Window/Renderer pattern.
deriving newtype instance MonadFail m => MonadFail (Apecs.SystemT world m)
