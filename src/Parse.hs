module Parse (mustParse) where

import Data.Text
import Data.Attoparsec.Text

mustParse :: Parser a -> Text -> a
mustParse p t =
  case parseOnly p t of
    Left err  -> error ("mustParse: " ++ err)
    Right x   -> x
