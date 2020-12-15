module Ruins.Script (Command (..), say, parseSayCommand) where

import qualified Apecs
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Linear
import Foreign.C.Types (CInt (..))
import Data.Vector (Vector)
import Control.Applicative ((<|>))
import Control.Lens ((&~), (.=), (+=), (<&>))
import Data.Void (Void)
import qualified Text.Megaparsec as MParsec
import qualified Text.Megaparsec.Char as MParsec
import qualified Text.Megaparsec.Char.Lexer as MParsec
import Ruins.Extra.SDL (mkRectangle)
import Ruins.Extra.Apecs (positionVector)
import Ruins.Miscellaneous (Name, mkName)
import Ruins.Components.Sprites (Sprite (..))
import Ruins.Components.World (RSystem, opened, currentText, sprite, voiceSound, letterDelay)

data Command = Walk Double Double Apecs.Entity
   | Say Text Double (Maybe Sprite) Name deriving stock Show

newtype Script = MkScript (Vector Command)

newtype Counter = MkCounter Int

type CommandParser = MParsec.Parsec Void Text Command

{-# Inline parseSayCommand #-}
parseSayCommand :: CommandParser
parseSayCommand = do
  MParsec.string "say"
  MParsec.space1

  text <-
    quoted (MParsec.some (MParsec.alphaNumChar <|> MParsec.oneOf @[] ",.!? "))
  MParsec.space1

  textDelay <- MParsec.float @_ @_ @_ @Double
  MParsec.space1

  -- | Doesn't work if there is no sprite
  sprite <- (Just <$> parseSprite) <|> pure Nothing
  MParsec.space1

  name <- parseName

  pure (Say (Text.pack text) textDelay sprite name)
  where quoted = MParsec.between (MParsec.char '\"') (MParsec.char '\"')
        parseName = mkName <$> quoted (MParsec.some MParsec.letterChar)
        parseRect = map (read @CInt) <$>
          MParsec.between (MParsec.char '[') (MParsec.char ']')
            (MParsec.sepEndBy1 (MParsec.some MParsec.digitChar) (MParsec.char ','))
          <&> \ (x : y : width : height) -> mkRectangle (x, y) (width, head height)

        parseSprite = MkSprite <$>
          MParsec.between (MParsec.char '(') (MParsec.char ')')
            ((,) <$> (parseName <* MParsec.char ',') <*> parseRect)

runCommand :: Command -> RSystem ()
runCommand = \ case
  Walk xDistance yDistance entity ->
    Apecs.modify entity \ position ->
      position &~ do
        positionVector . Linear._x += xDistance
        positionVector . Linear._y += yDistance

  Say text textDelay entityFace voiceName ->
    Apecs.set Apecs.global $ mempty &~ do
      opened .= True
      sprite .= entityFace
      voiceSound .= voiceName
      currentText .= text
      letterDelay .= textDelay

say :: Text -> Double -> Maybe Sprite -> Name -> RSystem ()
say text textDelay entityFace voiceName =
  Apecs.set Apecs.global $ mempty &~ do
    opened .= True
    currentText .= text
    sprite .= entityFace
    voiceSound .= voiceName
    letterDelay .= textDelay
