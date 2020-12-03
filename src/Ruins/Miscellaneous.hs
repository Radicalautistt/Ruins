module Ruins.Miscellaneous (
       Name
     , mkName
     , getName
     ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable)
import System.FilePath.Posix (dropExtension)

-- | Filename without file extension.
-- | Used as a key for ResourceMap.
newtype Name = MkName { getName :: Text }
  deriving stock Eq
  deriving newtype Hashable

{-# Inline mkName #-}
-- | Smart constructor for names
-- , making sure that extension is dropped.
mkName :: FilePath -> Name
mkName fileName = MkName (Text.pack (dropExtension fileName))

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withText "name" \ text ->
    -- | NOTE: this is dumb
    pure (mkName (Text.unpack text))
