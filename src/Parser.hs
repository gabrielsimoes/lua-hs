{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser where

import Token (lexeme, symbol, number, stringLit, boolean, varargs, parens, brackets)
import qualified Token

import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec(
    Parsec,
    (<?>),
    (<|>), unexpected, ErrorItem (Label),
  )
import Data.List.NonEmpty (fromList)

type Parser = Parsec Void Text

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
-- namelist ::= Name {`,´ Name}
-- explist1 ::= {exp `,´} exp
-- function ::= function funcbody
-- funcbody ::= `(´ [parlist1] `)´ block end
-- parlist1 ::= namelist [`,´ `...´]  |  `...´

data Stmt
  = AssignStmt [Var] [Expr]
  | FnCallStmt FnCall
  deriving (Show, Eq)

-- tableconstructor ::= `{´ [fieldlist] `}´
-- fieldlist ::= field {fieldsep field} [fieldsep]
-- fieldsep ::= `,´  |  `;´
newtype TableCons = NonEmpty Field
  deriving (Show, Eq)

-- field ::= `[´ exp `]´ `=´ exp  |  Name `=´ exp  |  exp
data Field
  = FieldBracket Expr Expr
  | FieldName Name Expr
  | FieldExpr Expr
  deriving (Show, Eq)

-- name
newtype Name = Name String
  deriving (Show, Eq)

name :: Parser Name
name = Name <$> Token.name

-- var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name
data Var = VarName Name
  | VarSubs Expr Expr -- prefixexp[exp]
  | VarMember Expr Name -- prefixexp.name
  deriving (Show, Eq)

var :: Parser Var
var = VarName <$> name
  <|> do
    p <- prefixExpr
    e <- brackets expr
    return $ VarSubs p e
--   <|> do
--     p <- prefixExpr
--     VarMember p <$> name

-- functioncall ::=  prefixexp args  |  prefixexp `:´ Name args
data FnCall = Expr Name Args
  deriving (Show, Eq)

functioncall :: Parser FnCall
functioncall = unexpected (Label $ fromList "TODO")

-- args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String
data Args =
  ArgList [Expr]
  | ArgTable TableCons
  | ArgString String
  deriving (Show, Eq)

args :: Parser Args
args = unexpected (Label $ fromList "TODO")

-- exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |
--          function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp
data Expr
  = ValueExpr Value
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | TableConsExpr TableCons
  | VarArgsExpr
  | FnCallExpr FnCall
  | VarExpr Var
  deriving (Show, Eq)

data Value =
  NilValue
  | BoolValue Bool
  | NumValue Double
  | StrValue String
  deriving (Show, Eq)

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

data UnOp = Neg | Not | Len
  deriving (Show, Eq)

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
  where
    term = nilExpr <|> boolExpr <|> numExpr <|> strExpr <|> varArgsExpr
          <|> functionExpr <|> prefixExpr <|> tableConsExpr <?> "term"

    nilExpr = ValueExpr NilValue <$ lexeme "nil"
    boolExpr = ValueExpr . BoolValue <$> boolean
    numExpr = ValueExpr . NumValue <$> number
    strExpr = ValueExpr . StrValue <$> stringLit
    varArgsExpr = VarArgsExpr <$ varargs
    functionExpr = unexpected (Label $ fromList "TODO")
    tableConsExpr = unexpected (Label $ fromList "TODO")

    binary sym binop = InfixL (BinOpExpr binop <$ symbol sym)
    unary sym unop = Prefix (UnOpExpr unop <$ symbol sym)
    opTable =
      [ [binary "^" Exp],
        [ unary "-" Neg,
          unary "not" Not,
          unary "#" Len
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

-- prefixexp ::= var  |  functioncall  |  `(´ exp `)´
prefixExpr :: Parser Expr
prefixExpr = VarExpr <$> var <|> (FnCallExpr <$> functioncall) <|> parens expr
