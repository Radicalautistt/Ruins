{-# Language FlexibleContexts #-}

module Ruins.Miscellaneous (
       Name
     , mkName
     , getName
     , afor_
     , emptyUArray
     ) where

import GHC.Int (Int32)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as SText
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Data.String (IsString (..))
import Data.Hashable (Hashable)
import Data.Ix (Ix (..))
import Data.Array.Unboxed (UArray)
import Data.Foldable (for_)
import Data.Array.IArray (IArray (..))
import qualified Data.Array.IArray as IArray
import qualified System.FilePath.Posix as FPath
import Control.Monad.IO.Class (MonadIO (..))

-- | Filename without file extension.
-- | Used mostly as a key for Ruins.Components.World.ResourceMap.
newtype Name = MkName { getName :: ShortText }
  deriving stock (Show, Eq)
  deriving newtype Hashable

{-# Inline mkName #-}
-- | Smart constructor for names
-- , making sure that extension is dropped.
mkName :: FilePath -> Name
mkName fileName = MkName (SText.pack (FPath.dropExtension fileName))

-- | This instance gives us the ability to write "megalovania"
-- | instead of (mkName "megalovania"), with mkName operating under the hood.
-- | Thus, even if we were to write "zalupa-kentavra.zip"
-- | the result would nevertheless be just "zalupa-kentavra".
instance IsString Name where
  fromString = mkName

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withText "name" \ text ->
    -- | NOTE: this is dumb
    pure (mkName (Text.unpack text))

emptyUArray :: (Num value, Ix value, IArray UArray value) => UArray value value
emptyUArray = IArray.listArray (0, 0) []

{-# Inline afor_ #-}
-- | for_ (flipped traverse_) over an unboxed array.
afor_ :: (Applicative f, Ix index, IArray UArray value) =>
         UArray index value -> (value -> f ()) -> f ()
afor_ array action = case bounds array of
  arrayBounds -> for_ (range arrayBounds) \ i ->
    action (array IArray.! i)

{-# Specialize afor_ :: MonadIO m =>
    UArray Int32 Int32 -> (Int32 -> m ()) -> m () #-}
