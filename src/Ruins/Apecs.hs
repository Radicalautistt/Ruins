{-# Options -fno-warn-orphans #-}
{-# Language StandaloneDeriving #-}
{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Ruins.Apecs where

import qualified Apecs
import qualified Apecs.Util as Apecs
import qualified Apecs.Physics as APhysics
import qualified Linear
import Foreign.C.Types (CInt)
import Control.Lens (Iso', iso)
import UnliftIO (MonadUnliftIO (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Managed (Managed, with, managed)
import Control.Monad.IO.Class (MonadIO)
import qualified Language.Haskell.TH as THaskell
import qualified Language.Haskell.TH.Syntax as THaskell

type ManagedSystem world result = Apecs.SystemT world Managed result

-- | We need this to actually use Window/Renderer pattern.
-- | from Ruins.Components module.
deriving newtype instance MonadFail m =>
  MonadFail (Apecs.SystemT world m)

-- | I defined these unliftio instances
-- | in order to freely call stuff like withAsync
-- , forConcurrently, etc, inside of the Apecs.SystemT.
-- | The fact that the aforementioned functions are all
-- | monomorphic in their monad (IO) was and is kind of annoying
-- , so after I've made some research I was seemingly left with two options:
-- | define either a MonadBaseControl IO instance (to use functions from lifted-async) or the MonadUnliftIO one.
-- | I've chosen the latter because I don't have IO as my base monad.
instance MonadUnliftIO Managed where
  {-# Inline withRunInIO #-}
  withRunInIO inner = managed \ callback ->
    callback =<< inner (flip with pure)

-- | Mostly copied it from ReaderT instance
-- | hackage.haskell.org/package/unliftio-core-0.2.0.1/docs/src/Control.Monad.IO.Unlift.html#line-64
instance MonadUnliftIO m =>
  MonadUnliftIO (Apecs.SystemT world m) where
  {-# Inline withRunInIO #-}
  withRunInIO inner = Apecs.SystemT $
    ReaderT \ world ->
      withRunInIO \ run ->
        inner (run . flip Apecs.runSystem world)

-- | Derive a global component instance if a given type is Monoid.
makeGlobalComponent :: THaskell.Name -> THaskell.DecQ
makeGlobalComponent componentName = do
 comp <- component
 hasMonoidInstance <- THaskell.isInstance ''Monoid [comp]
 if hasMonoidInstance
    then deriveGlobalInstance
         else fail (THaskell.showName componentName <> " doesn't have a monoid instance.")

 where component = THaskell.conT componentName
       deriveGlobalInstance = head <$>
         [d| instance Apecs.Component $component where
                 type Storage $component = Apecs.Global $component |]

{-# Inline newEntity_ #-}
-- | Make new entity without yelding the result.
-- | TODO remove this as soon as apecs gets new release
-- , since the function is already merged into the master branch.
newEntity_ :: MonadIO m =>
              Apecs.Set world m component =>
              Apecs.Get world m Apecs.EntityCounter =>
              component -> Apecs.SystemT world m ()

newEntity_ component = do
  entity <- Apecs.nextEntity
  Apecs.set entity component

{-# Inline mkPosition #-}
mkPosition :: Double -> Double -> APhysics.Position
mkPosition x y = APhysics.Position (Linear.V2 x y)

{-# Inline mkVelocity #-}
mkVelocity :: Double -> Double -> APhysics.Velocity
mkVelocity x y = APhysics.Velocity (Linear.V2 x y)

unitPosition :: APhysics.Position
unitPosition = mkPosition 0 0

unitVelocity :: APhysics.Velocity
unitVelocity = mkVelocity 0 0

{-# Complete XY #-}
-- | Wrapper around Position, used to clean up pattern matching.
pattern XY :: Double -> Double -> APhysics.Position
pattern XY x y <- APhysics.Position (Linear.V2 x y)

{-# Complete VEL #-}
pattern VEL :: Double -> Double -> APhysics.Velocity
pattern VEL x y <- APhysics.Velocity (Linear.V2 x y)

{-# Complete RXY #-}
-- | Same as XY. but with Doubles coerced into CInts.
-- | Defined solely to ease the usage of SDL.copy
-- , since it requires CInts in rendering rectangles.
pattern RXY :: CInt -> CInt -> APhysics.Position
pattern RXY x y <-
  APhysics.Position (Linear.V2 (round -> x) (round -> y))

{-# Inline velocityVector #-}
-- | Isomorphism between Velocity and (V2 Double).
-- | I need it so I could easily get/set values inside Velocity.
-- | There is probably a way to do the same thing with Control.Lens.coerced
-- , but I haven't figured out how yet (at least the setting part).
velocityVector :: Iso' APhysics.Velocity (Linear.V2 Double)
velocityVector = iso (\ (APhysics.Velocity vector) -> vector) APhysics.Velocity
