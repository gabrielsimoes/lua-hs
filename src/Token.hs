{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Token where

import Control.Monad (void)
import Control.Monad.Combinators (skipCount)
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.Scientific (toRealFloat, Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P),
    Parsec,
    anySingle,
    between,
    many,
    manyTill,
    notFollowedBy,
    skipManyTill,
    takeWhileP,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (char, digitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

-- space consumer
sc :: Lexer ()
sc = L.space space1 skipLineComment skipLuaBlockComment

-- parse a token followed by space
lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

-- parse a fixed symbol
symbol :: Text -> Lexer Text
symbol = L.symbol sc

-- match lua comment blocks --[=[ ]=] and string blocks [=[ ]=] with matching number of equals
luaBlock :: Text -> (Lexer () -> Lexer a) -> Lexer a
luaBlock prefix bodyTill = do
  _ <- string prefix
  n <- try start
  bodyTill (try $ end n)
  where
    start = do
      _ <- char '['
      n <- length <$> many (char '=')
      _ <- char '['
      return n
    end n = void $ char ']' >> skipCount n (char '=') >> char ']'

skipLuaBlockComment :: Lexer ()
skipLuaBlockComment = luaBlock "--" (skipManyTill anySingle)

skipLineComment :: Lexer ()
skipLineComment = do
  _ <- try $ char '-' <* char '-' <* notFollowedBy (char '[')
  _ <- takeWhileP (Just "any character") (/= '\n')
  return ()

-- string literals
stringLit :: Lexer String
stringLit = lexeme (singleLine '"') <|> lexeme (singleLine '\'') <|> lexeme multiLine
  where
    singleLine q = char q >> manyTill (notFollowedBy (char '\n') *> L.charLiteral) (char q)
    multiLine = removeFirstNewline <$> luaBlock "" (manyTill L.charLiteral)
    removeFirstNewline [] = ""
    removeFirstNewline (x : xs) =
      if x == '\n'
        then xs
        else x : xs

number :: Lexer Scientific
number = lexeme L.scientific

boolean :: Lexer Bool
boolean = (False <$ lexeme "false") <|> (True <$ lexeme "true")

parens, squareBrackets, curlyBrackets :: Lexer a -> Lexer a
parens = between (symbol "(") (symbol ")")
squareBrackets = between (symbol "[") (symbol "]")
curlyBrackets = between (symbol "{") (symbol "}")

varargs :: Lexer ()
varargs = void $ lexeme "..."

fieldsep :: Lexer ()
fieldsep = void $ lexeme (char ',') <|> lexeme (char ';')

-- letters, digits, underscore, not starting with digit
identifier :: Lexer String
identifier = notFollowedBy digitChar *> (lexeme . try) (id >>= notReserved) <&> T.unpack
  where
    id = takeWhile1P (Just "name character") isAlphaNumDash
    isAlphaNumDash a = isAlphaNum a || a == '_'
    notReserved w =
      if w `elem` reservedKeywords
        then fail $ "keyword " <> show w <> " cannot be a name"
        else return w

reservedKeywords :: [Text]
reservedKeywords =
  [ "nil",
    "true",
    "false",
    "function",
    "do",
    "end",
    "if",
    "then",
    "elseif",
    "else",
    "while",
    "repeat",
    "until",
    "for",
    "local",
    "in",
    "return",
    "break",
    "and",
    "or",
    "not"
  ]
