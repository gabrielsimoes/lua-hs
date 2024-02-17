{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Token where

import Control.Monad (void)
import Control.Monad.Combinators (skipCount)
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.Scientific (toRealFloat)
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

data Token
  = Nil
  | TrueLit
  | FalseLit
  | Function
  | Do
  | End
  | If
  | Then
  | Elseif
  | Else
  | While
  | Repeat
  | Until
  | For
  | Local
  | In
  | Return
  | Break
  | Assign
  | Plus
  | Minus
  | Mul
  | Div
  | Exp
  | Mod
  | Concat
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Neq
  | And
  | Or
  | Not
  | VarArgs
  | Len
  | SqOpen
  | SqClose
  | ParenOpen
  | ParenClose
  | FieldSep
  | Name String
  | NumberLit Double
  | StringLit String
  deriving (Eq, Show)

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
  _ <- takeWhileP (Just "character") (/= '\n')
  return ()

-- string literals
stringLit :: Lexer String
stringLit = lexeme (singleLine '"') <|> lexeme (singleLine '\'') <|> lexeme multiLine
  where
    singleLine q = char q >> manyTill (L.charLiteral <* notFollowedBy (char '\n')) (char q)
    multiLine = removeFirstNewline <$> luaBlock "" (manyTill L.charLiteral)
    removeFirstNewline [] = ""
    removeFirstNewline (x : xs) =
      if x == '\n'
        then xs
        else x : xs

-- lua numbers are always doubles
number :: Lexer Double
number = fmap toRealFloat (lexeme L.scientific)

boolean :: Lexer Bool
boolean = (False <$ lexeme "false") <|> (True <$ lexeme "true")

parens :: Lexer a -> Lexer a
parens = between (symbol "(") (symbol ")")

brackets :: Lexer a -> Lexer a
brackets = between (symbol "[") (symbol "]")

varargs :: Lexer Text
varargs = lexeme "..."

fieldsep :: Lexer ()
fieldsep = void $ lexeme (char ',') <|> lexeme (char ';')

-- letters, digits, underscore, not starting with digit
name :: Lexer String
name =
  notFollowedBy digitChar
    *> lexeme (takeWhile1P (Just "name") isAlphaNumDash)
    <&> T.unpack
  where
    isAlphaNumDash a = isAlphaNum a || a == '_'

lexer :: Lexer [Token]
lexer =
  many $
    fmap StringLit stringLit
      <|> fmap NumberLit number
      <|> foldl1
        (<|>)
        ( (\(s, t) -> symbol s >> pure t)
            <$> [ ("nil", Nil),
                  ("true", TrueLit),
                  ("false", FalseLit),
                  ("function", Function),
                  ("do", Do),
                  ("end", End),
                  ("if", If),
                  ("then", Then),
                  ("elseif", Elseif),
                  ("else", Else),
                  ("while", While),
                  ("repeat", Repeat),
                  ("until", Until),
                  ("for", For),
                  ("local", Local),
                  ("in", In),
                  ("return", Return),
                  ("break", Break),
                  ("=", Assign),
                  ("+", Plus),
                  ("-", Minus),
                  ("*", Mul),
                  ("/", Div),
                  ("^", Exp),
                  ("%", Mod),
                  ("..", Concat),
                  ("<", Lt),
                  ("<=", Lte),
                  (">", Gt),
                  (">=", Gte),
                  ("==", Eq),
                  ("~=", Neq),
                  ("and", And),
                  ("or", Or),
                  ("not", Not),
                  ("#", Len),
                  ("...", VarArgs),
                  ("[", SqOpen),
                  ("]", SqClose),
                  ("(", ParenOpen),
                  (")", ParenClose),
                  (",", FieldSep),
                  (";", FieldSep)
                ]
        )
      <|> fmap Name name
