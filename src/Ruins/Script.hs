module Ruins.Script (
       say
     , runCommand
     , parseScripts
     ) where

import qualified Apecs
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Linear
import Foreign.C.Types (CInt (..))
import Control.Applicative ((<|>))
import Control.Lens ((&~), (.=), (+=), (<&>))
import Data.Void (Void)
import qualified Data.Vector as Vector
import qualified Text.Megaparsec as MParsec
import qualified Text.Megaparsec.Char as MParsec
import qualified Text.Megaparsec.Char.Lexer as MParsec
import Ruins.Extra.SDL (mkRectangle)
import Ruins.Extra.Apecs (positionVector)
import Ruins.Miscellaneous (Name, mkName)
import Ruins.Components.Script (Command (..), Script (..))
import Ruins.Components.Sprites (Sprite (..))
import Ruins.Components.World (RSystem, opened, currentText, sprite, voiceSound, letterDelay)

type CommandParser = MParsec.Parsec Void Text Command

{-# Inline parseSayCommand #-}
parseSayCommand :: CommandParser
parseSayCommand = do
  MParsec.string "say"
  MParsec.space1

  text <- between '\"' '\"'
    (MParsec.some (MParsec.alphaNumChar <|> MParsec.oneOf @[] ",.!? "))
  MParsec.space1

  textDelay <- MParsec.float @_ @_ @_ @Double
  MParsec.space1

  -- | Doesn't work if there is no sprite
  entityFace <- (Just <$> parseSprite) <|> pure Nothing
  MParsec.space1

  voiceName <- parseName

  pure (Say (Text.pack text) textDelay entityFace voiceName)
  where between start end parser = MParsec.between (MParsec.char start) (MParsec.char end) parser
        parseName = mkName <$> between '\"' '\"' (MParsec.some (MParsec.letterChar <|> MParsec.char '-'))
        parseRect = map (read @CInt) <$>
          between '[' ']'
            (MParsec.sepEndBy1 (MParsec.some MParsec.digitChar) (MParsec.char ','))
          <&> \ (x : y : width : height) -> mkRectangle (x, y) (width, head height)

        parseSprite = MkSprite <$>
          between '(' ')' ((,) <$> (parseName <* MParsec.char ',') <*> parseRect)

{-# Inline parseCommand #-}
parseCommand :: CommandParser
parseCommand = MParsec.choice [
  parseSayCommand
  ]

{-# Inline parseScript #-}
parseScript :: MParsec.Parsec Void Text Script
parseScript = do
  _scriptOwner <-
    mkName <$> MParsec.some (MParsec.letterChar <|> MParsec.char '-')
  MParsec.newline
  _scriptData <-
    Vector.fromList <$> MParsec.sepEndBy1 parseCommand MParsec.newline
  let _scriptCounter = Vector.length _scriptData
  pure MkScript {..}

{-# Inline parseScripts #-}
-- | Parse the contents of a file containing scripts.
parseScripts :: MParsec.Parsec Void Text [Script]
parseScripts = MParsec.sepEndBy1 parseScript MParsec.newline

runCommand :: Command -> RSystem ()
runCommand = \ case
  Walk _ xDistance yDistance entity ->
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
