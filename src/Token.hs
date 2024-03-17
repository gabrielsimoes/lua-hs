{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Token where

import Control.Monad (ap, liftM, void)
import Control.Monad.Combinators (count, skipCount)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isHexDigit)
import Data.Foldable (foldl')
import qualified Data.Foldable1 as T
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.NonEmpty (fromList)
import Data.Maybe (catMaybes)
import Data.Numbers.FloatingHex (readHFloat)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Internal.Builder (fromText)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Base (Alternative (some), getTag, unsafeChr)
import Numeric (readFloat, readHex)
import Text.Megaparsec (ErrorItem (Label), MonadParsec (takeWhile1P), Parsec, anySingle, between, choice, lookAhead, many, manyTill, notFollowedBy, oneOf, option, optional, satisfy, skipMany, skipManyTill, takeWhileP, try, unexpected, (<?>), (<|>))
import Text.Megaparsec.Char (char, char', digitChar, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ErrorItem (Tokens))
import Text.Megaparsec.Stream (chunkToTokens)
import Prelude hiding (exponent)
import Data.Text (Text)

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
  _ <- try $ char '-' <* char '-' <* notFollowedBy (char '[' >> skipMany (char '=') >> char '[')
  _ <- takeWhileP (Just "any character") (/= '\n')
  return ()

-- string literals
stringLit :: Lexer ByteString
stringLit =
  lexeme $
    choice
      [ singleLine (char '"'),
        singleLine (char '\''),
        multiLine
      ]
  where
    singleLine :: Lexer a -> Lexer ByteString
    singleLine delim = delim >> (BS.concat <$> manyTill singleLineToken delim)
    -- TODO: can this be made faster?
    singleLineTokens :: Lexer a -> Lexer [ByteString]
    singleLineTokens = manyTill singleLineToken
    singleLineToken :: Lexer ByteString
    singleLineToken =
      notFollowedBy (char '\n')
        *> choice
          [ encodeUtf8 "\n" <$ try (string "\\\n"),
            BS.empty <$ try (string "\\z" <* sc),
            codepointToUtf8 <$> try (string "\\u{" *> L.hexadecimal <* char '}'),
            BS.singleton . hexStringToWord <$> try (string "\\x" *> count 2 hexDigitChar),
            encodeUtf8 . T.singleton <$> L.charLiteral
          ]
    multiLine = encodeUtf8 . T.pack . removeFirstNewline <$> luaBlock "" (manyTill anySingle)
    removeFirstNewline [] = ""
    removeFirstNewline (x : xs) =
      if x == '\n'
        then xs
        else x : xs
    codepointToUtf8 :: Int -> ByteString
    codepointToUtf8 = encodeUtf8 . T.singleton . unsafeChr
    hexStringToWord :: String -> Char
    hexStringToWord = chr . fst . (!! 0) . readHex

number :: Lexer (Either Int64 Double)
number = do
  lexeme $
    choice
      [ Left <$> try integer,
        Right <$> hexfloat,
        Right <$> float
      ]
  where
    integer, hexadecimal :: Lexer Int64
    integer = (hexadecimal <|> L.decimal) <* notFollowedBy (oneOf ['e', 'E', 'p', 'P', '.'])
    hexadecimal = try (char '0' >> char' 'x') >> L.hexadecimal
    float, hexfloat :: Lexer Double
    -- handle .1 and 1.
    float = try $ do
      whole <- many digitChar
      _ <- lookAhead (char' 'e' <|> char '.') -- has to be a float
      frac <- option "" $ liftA2 (:) (char '.') (digits' $ null whole)
      exp <- option "" $ do
        e <- char' 'e'
        sign <- optional $ satisfy (\c -> c == '+' || c == '-')
        val <- digits' True
        return $ [e] ++ maybe "" (: []) sign ++ val
      let literal = concat ["0", whole, ".", frac, "0", exp]
      return $ fst . (!! 0) . readFloat $ literal
    hexfloat = do
      zerox <- try $ char '0' >> char' 'x' <&> \x -> "0" ++ [x]
      whole <- hexdigits' False
      frac <- option "" $ liftA2 (:) (char '.') (hexdigits' $ null whole)
      exp <- option "p0" $ do
        p <- char' 'p'
        sign <- optional $ satisfy (\c -> c == '+' || c == '-')
        val <- digits' True
        return $ [p] ++ maybe "" (: []) sign ++ val
      let literal = concat [zerox, whole, frac, exp]
      case readHFloat literal of
        Nothing -> unexpected $ Tokens $ fromList literal
        Just f -> return f
    digits' atLeast1 =
      T.unpack
        <$> (if atLeast1 then takeWhile1P else takeWhileP) (Just "digit") isDigit
    hexdigits' atLeast1 =
      T.unpack
        <$> (if atLeast1 then takeWhile1P else takeWhileP) (Just "hex digit") isHexDigit

boolean :: Lexer Bool
boolean = (False <$ lexeme "false") <|> (True <$ lexeme "true")

parens, squareBrackets, curlyBrackets :: Lexer a -> Lexer a
parens = between (symbol "(") (symbol ")")
squareBrackets = between openBracket closeBracket
  where
    openBracket = try $ lexeme (char '[' <* notFollowedBy (skipMany (char '=') *> char '['))
    closeBracket = symbol "]"
curlyBrackets = between (symbol "{") (symbol "}")

varargs :: Lexer ()
varargs = void $ lexeme "..."

fieldsep :: Lexer ()
fieldsep = void $ lexeme (char ',') <|> lexeme (char ';')

-- letters, digits, underscore, not starting with digit
identifier :: Lexer String
identifier = notFollowedBy digitChar *> (lexeme . try) (id >>= notReserved) <&> T.unpack
  where
    id = takeWhile1P (Just "identifier") isAlphaNumDash
    isAlphaNumDash a = isAlphaNum a || a == '_'
    notReserved w =
      if w `elem` reservedKeywords
        then unexpected $ Tokens $ fromList $ T.unpack w
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
    "not",
    "goto"
  ]
