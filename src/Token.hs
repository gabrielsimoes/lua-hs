{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Token (expr) where

import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, (<?>), (<|>), manyTill)
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as L

-- data Expr = NumberExp Double
--           | StringExp String
--           | VarExp String
--           | BinOpExp String Expr Expr
--           | FunctionCallExp [Expr]
--           | TableExp [(String, Expr)]
--           | FieldSep
--           deriving (Show, Eq)

-- tokenizer =
--   P.makeTokenParser
--     LanguageDef
--       { commentStart = "--[[",
--         commentEnd = "]]",
--         commentLine = "--",
--         nestedComments = False,
--         identStart = letter <|> char '_',
--         identLetter = alphaNum <|> char '_',
--         opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
--         opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
--         reservedNames = [],
--         reservedOpNames = [],
--         caseSensitive = True
--       }

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
  | ThreeDots
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
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "--[[" "]]")

-- parse a token followed by space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse a fixed symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- lua numbers are always doubles
number :: Parser Double
number = fmap toRealFloat (lexeme L.scientific)

-- string literals
string :: Parser String
string = lexeme doubleQuotes <|> lexeme singleQuotes <|> lexeme multiLine
  where
    doubleQuotes = char '"' >> manyTill L.charLiteral (char '"')
    singleQuotes = char '\'' >> manyTill L.charLiteral (char '\'')
    multiLine = undefined -- TODO: implement multiline string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
  where
    term = parens expr <|> numExpr <|> strExpr <?> "term"
    numExpr = fmap NumExpr number
    strExpr = fmap StrExpr string
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
    binary sym binop = InfixL ((`BinOpExpr` binop) <$ symbol sym)
    unary sym unop = Prefix (UnOpExpr unop <$ symbol sym)
