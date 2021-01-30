{-# Options -fno-warn-orphans #-}
{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}

-- | This module exists to extend the interface
-- | provided by the apecs and apecs-physics packages.
module Ruins.Extra.Apecs where

import qualified Apecs
import qualified Apecs.Util as Apecs
import qualified Apecs.Physics as Physics
import qualified Linear
import qualified Data.Aeson as Aeson
import Foreign.C.Types (CInt)
import Control.Lens (Iso', iso)
import Control.Monad.Managed (Managed, MonadManaged (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.IO.Class (MonadIO)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

-- | A System that gives a promise of releasing all the allocated resources
-- | once a program terminates. The Managed monad is being similar to ResourceT
-- , albeit much more simple to use.
type ManagedSystem world result = Apecs.SystemT world Managed result
deriving newtype instance MonadFail m => MonadFail (Apecs.SystemT world m)
deriving newtype instance MonadManaged m => MonadManaged (Apecs.SystemT world m)

instance Aeson.FromJSON Physics.Position where
  parseJSON = Aeson.withArray "position" \ position -> do
    [x, y] <- Aeson.parseJSON @[Double] (Aeson.Array position)
    pure (mkPosition x y)

-- | Derive a global component instance if a given type is Monoid.
makeGlobalComponent :: TH.Name -> TH.DecQ
makeGlobalComponent componentName = do
 comp <- component
 hasMonoidInstance <- TH.isInstance ''Monoid [comp]
 if hasMonoidInstance
    then deriveGlobalInstance
         else fail (TH.showName componentName <> " doesn't have a monoid instance.")

 where component = TH.conT componentName
       deriveGlobalInstance = head <$>
         [d| instance Apecs.Component $component where
                 type Storage $component = Apecs.Global $component |]

{-# Inline newEntity_ #-}
-- | Make new entity without yelding the result.
-- | TODO remove this as soon as apecs gets new release on hackage
-- , since the function is already merged into the master branch.
newEntity_ :: MonadIO m =>
              Apecs.Set world m component =>
              Apecs.Get world m Apecs.EntityCounter =>
              component -> Apecs.SystemT world m ()

newEntity_ component = do
  entity <- Apecs.nextEntity
  entity Apecs.$= component

{-# Inline mkPosition #-}
mkPosition :: Double -> Double -> Physics.Position
mkPosition x y = Physics.Position (Linear.V2 x y)

{-# Inline mkVelocity #-}
mkVelocity :: Double -> Double -> Physics.Velocity
mkVelocity x y = Physics.Velocity (Linear.V2 x y)

unitPosition :: Physics.Position
unitPosition = mkPosition 0 0

unitVelocity :: Physics.Velocity
unitVelocity = mkVelocity 0 0
 
deriving newtype instance Num Physics.Position
deriving newtype instance Num Physics.Velocity

{-# Complete XY #-}
-- | Wrapper around Position, used to clean up pattern matching.
pattern XY :: Double -> Double -> Physics.Position
pattern XY x y <- Physics.Position (Linear.V2 x y)

{-# Complete VEL #-}
pattern VEL :: Double -> Double -> Physics.Velocity
pattern VEL x y <- Physics.Velocity (Linear.V2 x y)

{-# Complete RXY #-}
-- | Same as XY. but with Doubles coerced into CInts.
-- | Defined solely to ease the usage of SDL.copy
-- , since it requires CInts in rendering rectangles.
pattern RXY :: CInt -> CInt -> Physics.Position
pattern RXY x y <-
  Physics.Position (Linear.V2 (round -> x) (round -> y))

{-# Inline positionVector #-}
-- | Isomorphisms between Velocity/Position and (V2 Double).
-- | I need them so I could easily get/set values inside Velocity/Position.
-- | There is probably a better way to do this with Control.Lens.coerced
-- , but I haven't figured it out just yet (at least the setting part).
positionVector :: Iso' Physics.Position (Linear.V2 Double)
positionVector = iso (\ (Physics.Position vector) -> vector) Physics.Position

{-# Inline velocityVector #-}
velocityVector :: Iso' Physics.Velocity (Linear.V2 Double)
velocityVector = iso (\ (Physics.Velocity vector) -> vector) Physics.Velocity
