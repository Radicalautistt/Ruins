module Ruins.Script (
       say
     , runCommand
     , parseScripts
     ) where

import qualified Apecs
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Linear
import qualified Data.Vector as Vector
import Foreign.C.Types (CInt (..))
import Control.Applicative ((<|>))
import Control.Lens ((&~), (.=), (+=), (<&>))
import Data.Void (Void)
import qualified Text.Megaparsec as MParsec
import qualified Text.Megaparsec.Char as MParsec
import qualified Text.Megaparsec.Char.Lexer as MParsec
import qualified Ruins.Extra.SDL as ESDL
import qualified Ruins.Extra.Apecs as EApecs
import qualified Ruins.Miscellaneous as Misc
import qualified Ruins.Components.World as World
import qualified Ruins.Components.Script as Script
import qualified Ruins.Components.Sprites as Sprites

type CommandParser = MParsec.Parsec Void Text Script.Command

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
  -- entityFace <- (Just <$> parseSprite) <|> pure Nothing
  MParsec.space1

  voiceName <- parseName

  pure (Script.Say (Text.pack text) textDelay undefined voiceName)
  where between start end parser = MParsec.between (MParsec.char start) (MParsec.char end) parser
        parseName = Misc.mkName <$> between '\"' '\"' (MParsec.some (MParsec.letterChar <|> MParsec.char '-'))
        -- parseRect = map (read @CInt) <$>
        --   between '[' ']'
        --     (MParsec.sepEndBy1 (MParsec.some MParsec.digitChar) (MParsec.char ','))
        --   <&> \ (x : y : width : height) -> ESDL.mkRectangle (x, y) (width, head height)

        -- parseSprite = Sprites.Sprite <$>
        --   between '(' ')' ((,) <$> (parseName <* MParsec.char ',') <*> parseRect)

{-# Inline parseCommand #-}
parseCommand :: CommandParser
parseCommand = MParsec.choice [
  parseSayCommand
  ]

{-# Inline parseScript #-}
parseScript :: MParsec.Parsec Void Text Script.Script
parseScript = do
  _scriptOwner <-
    Misc.mkName <$> MParsec.some (MParsec.letterChar <|> MParsec.char '-')
  MParsec.newline
  _scriptData <-
    Vector.fromList <$> MParsec.sepEndBy1 parseCommand MParsec.newline
  let _scriptCounter = Vector.length _scriptData
  pure Script.Script {..}

{-# Inline parseScripts #-}
-- | Parse the contents of a file containing scripts.
parseScripts :: MParsec.Parsec Void Text [Script.Script]
parseScripts = MParsec.sepEndBy1 parseScript MParsec.newline

runCommand :: Script.Command -> World.RSystem ()
runCommand = \ case
  Script.Walk _ xDistance yDistance entity ->
    Apecs.modify entity \ position ->
      position &~ do
        EApecs.positionVector . Linear._x += xDistance
        EApecs.positionVector . Linear._y += yDistance

  Script.Say text textDelay entityFace voiceName ->
    Apecs.set Apecs.global $ mempty &~ do
      World.opened .= True
      World.sprite .= entityFace
      World.voiceSound .= voiceName
      World.currentText .= text
      World.letterDelay .= textDelay

runScript :: Script.Script -> World.RSystem ()
runScript Script.Script {..} =
  runCommand (_scriptData Vector.! _scriptCounter)

say :: Text -> Double -> Maybe Sprites.Sprite -> Misc.Name -> World.RSystem ()
say text textDelay entityFace voiceName =
  Apecs.set Apecs.global $ mempty &~ do
    World.opened .= True
    World.currentText .= text
    World.sprite .= entityFace
    World.voiceSound .= voiceName
    World.letterDelay .= textDelay
