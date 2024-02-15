{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Token (expr) where

import Control.Monad (void)
import Control.Monad.Combinators (skipCount)
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    anySingle,
    between,
    many,
    manyTill,
    notFollowedBy,
    skipManyTill,
    takeWhileP,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- data Expr = NumberExp Double
--           | StringExp String
--           | VarExp String
--           | BinOpExp String Expr Expr
--           | FunctionCallExp [Expr]
--           | TableExp [(String, Expr)]
--           | FieldSep
--           deriving (Show, Eq)

-- chunk ::= {stat [`;´]} [laststat[`;´]]
-- block ::= chunk
-- stat ::=  varlist1 `=´ explist1  |
--          functioncall  |
--          do block end  |
--          while exp do block end  |
--          repeat block until exp  |
--          if exp then block {elseif exp then block} [else block] end  |
--          for Name `=´ exp `,´ exp [`,´ exp] do block end  |
--          for namelist in explist1 do block end  |
--          function funcname funcbody  |
--          local function Name funcbody  |
--          local namelist [`=´ explist1]
-- laststat ::= return [explist1]  |  break
-- funcname ::= Name {`.´ Name} [`:´ Name]
-- varlist1 ::= var {`,´ var}
-- var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name
-- namelist ::= Name {`,´ Name}
-- explist1 ::= {exp `,´} exp
-- exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |
--          function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp

data Expr
  = Nil
  | BoolExpr Bool
  | NumExpr Double
  | StrExpr String
  | VarArgsExpr -- ...
  | BinOpExpr Expr BinOp Expr
  | UnOpExpr UnOp Expr
  deriving (Show, Eq)

-- function
-- prefixexp
-- tableconstructor

-- prefixexp ::= var  |  functioncall  |  `(´ exp `)´
-- functioncall ::=  prefixexp args  |  prefixexp `:´ Name args
-- args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String
-- function ::= function funcbody
-- funcbody ::= `(´ [parlist1] `)´ block end
-- parlist1 ::= namelist [`,´ `...´]  |  `...´
-- tableconstructor ::= `{´ [fieldlist] `}´
-- fieldlist ::= field {fieldsep field} [fieldsep]
-- field ::= `[´ exp `]´ `=´ exp  |  Name `=´ exp  |  exp
-- fieldsep ::= `,´  |  `;´

data BinOp
  = Add
  | Sub
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
  deriving (Show, Eq)

data UnOp = Neg | Not | Hash
  deriving (Show, Eq)

-- parser functions
type Parser = Parsec Void Text

-- space consumer
sc :: Parser ()
sc = L.space space1 skipLineComment skipLuaBlockComment

-- parse a token followed by space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse a fixed symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- match lua comment blocks --[=[ ]=] and string blocks [=[ ]=] with matching number of equals
luaBlock :: Text -> (Parser () -> Parser a) -> Parser a
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

skipLuaBlockComment :: Parser ()
skipLuaBlockComment = luaBlock "--" (skipManyTill anySingle)

skipLineComment :: Parser ()
skipLineComment = do
  _ <- try $ char '-' <* char '-' <* notFollowedBy (char '[')
  _ <- takeWhileP (Just "character") (/= '\n')
  return ()

-- string literals
stringLit :: Parser String
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
number :: Parser Double
number = fmap toRealFloat (lexeme L.scientific)

boolean :: Parser Bool
boolean = (False <$ lexeme "false") <|> (True <$ lexeme "true")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

varargs :: Parser Text
varargs = lexeme "..."

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
  where
    term = parens expr <|> nilExpr <|> boolExpr <|> numExpr <|> strExpr <|> varArgsExpr <?> "term"
    nilExpr = Nil <$ lexeme "nil"
    boolExpr = fmap BoolExpr boolean
    numExpr = fmap NumExpr number
    strExpr = fmap StrExpr stringLit
    varArgsExpr = VarArgsExpr <$ varargs
    binary sym binop = InfixL ((`BinOpExpr` binop) <$ symbol sym)
    unary sym unop = Prefix (UnOpExpr unop <$ symbol sym)
    opTable =
      [ [binary "^" Exp],
        [ unary "-" Neg,
          unary "not" Not,
          unary "#" Hash
        ],
        [ binary "*" Mul,
          binary "/" Div,
          binary "%" Mod
        ],
        [ binary "+" Add,
          binary "-" Sub
        ],
        [binary ".." Concat],
        [ binary "<" Lt,
          binary ">" Gt,
          binary "<=" Lte,
          binary ">=" Gte,
          binary "~=" Neq,
          binary "==" Eq
        ],
        [ binary "and" And
        ],
        [binary "or" Or]
      ]
