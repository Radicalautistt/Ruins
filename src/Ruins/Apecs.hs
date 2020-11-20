{-# Language ViewPatterns #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Ruins.Apecs where

import qualified SDL
import qualified Apecs
import qualified Apecs.Util as Apecs
import qualified Apecs.Physics as APhysics
import Foreign.C.Types (CInt)
import Control.Monad.IO.Class (MonadIO)
import qualified Language.Haskell.TH as THaskell
import qualified Language.Haskell.TH.Syntax as THaskell

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
newEntity_ :: MonadIO m =>
              Apecs.Set world m component =>
              Apecs.Get world m Apecs.EntityCounter =>
              component -> Apecs.SystemT world m ()

newEntity_ component = do
  entity <- Apecs.nextEntity
  Apecs.set entity component

{-# Inline mkPosition #-}
mkPosition :: Double -> Double -> APhysics.Position
mkPosition x y = APhysics.Position (SDL.V2 x y)

{-# Inline mkVelocity #-}
mkVelocity :: Double -> Double -> APhysics.Velocity
mkVelocity x y = APhysics.Velocity (SDL.V2 x y)

unitPosition :: APhysics.Position
unitPosition = mkPosition 0 0

unitVelocity :: APhysics.Velocity
unitVelocity = mkVelocity 0 0

{-# Complete XY #-}
-- | Wrapper around Position, used to clean up pattern matching.
pattern XY :: Double -> Double -> APhysics.Position
pattern XY x y <- APhysics.Position (SDL.V2 x y)

{-# Complete VEL #-}
pattern VEL :: Double -> Double -> APhysics.Velocity
pattern VEL x y <- APhysics.Velocity (SDL.V2 x y)

{-# Complete RXY #-}
-- | Same as XY. but with Doubles coerced into CInts.
-- | Defined solely to ease the usage of SDL.copy
-- , since it requires CInts in rendering rectangles.
pattern RXY :: CInt -> CInt -> APhysics.Position
pattern RXY x y <-
  APhysics.Position (SDL.V2 (round -> x) (round -> y))
