{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators (some)
import Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix), makeExprParser)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( ErrorItem (Label),
    MonadParsec (notFollowedBy),
    Parsec,
    many,
    oneOf,
    optional,
    satisfy,
    sepBy,
    sepBy1,
    sepEndBy,
    try,
    unexpected,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Debug (dbg, dbg')
import Token
  ( boolean,
    curlyBrackets,
    identifier,
    lexeme,
    number,
    parens,
    sc,
    squareBrackets,
    stringLit,
    symbol,
    varargs,
  )
import qualified Token

type Parser = Parsec Void Text

(>*<) :: Parser a -> Parser b -> Parser (a, b)
pa >*< pb = do
  a <- pa
  b <- pb
  return (a, b)

program :: Parser [Stmt]
program = sc >> block

-- name
newtype Name = Name String
  deriving (Show, Eq)

name :: Parser Name
name = Name <$> identifier

-- funcname ::= Name {`.´ Name} [`:´ Name]
data FuncName = FuncName (NonEmpty Name) (Maybe Name)
  deriving (Show, Eq)

funcname :: Parser FuncName
funcname = do
  names <- sepBy1 name (symbol ".")
  method <- optional $ symbol ":" >> name
  return $ FuncName (fromList names) method

-- namelist ::= Name {`,´ Name}
namelist = sepBy name (symbol ",") <?> "namelist"

namelist1 = sepBy1 name (symbol ",") <?> "namelist1"

-- varlist1 ::= var {`,´ var}
varlist1 = sepBy1 var (symbol ",") <?> "varlist1"

-- explist1 ::= {exp `,´} exp
exprlist = sepBy expr (symbol ",") <?> "exprlist"

exprlist1 = sepBy1 expr (symbol ",") <?> "exprlist1"

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

data Stmt
  = AssignStmt [Var] [Expr]
  | FnCallStmt FnCall
  | BlockStmt [Stmt]
  | WhileDoStmt Expr [Stmt]
  | RepeatUntilStmt [Stmt] Expr
  | ConditionalStmt [(Expr, [Stmt])] [Stmt]
  | ForListStmt Name [Expr] [Stmt]
  | ForRangeStmt [Name] [Expr] [Stmt]
  | FunctionStmt FuncName Function
  | LocalFunctionStmt Name Function
  | LocalAssignStmt [Name] [Expr]
  | ReturnStmt [Expr]
  | BreakStmt
  deriving (Show, Eq)

stmt :: Parser Stmt
stmt =
  dbg
    "stmt"
    ( foldl1
        (<|>)
        [ whileDoStmt,
          conditionalStmt,
          try assignStmt,
          try fnCallStmt,
          blockStmt,
          repeatUntilStmt,
          forListStmt,
          forRangeStmt,
          functionStmt,
          localStmt,
          returnStmt,
          breakStmt
        ]
        <?> "statement"
    )
  where
    assignStmt = dbg "assign stmt" $ do
      vars <- varlist1
      _ <- symbol "="
      AssignStmt vars <$> exprlist1
    fnCallStmt = dbg "fncall stmt" $ FnCallStmt <$> functioncall
    blockStmt = dbg "block stmt" $ BlockStmt <$> (symbol "do" *> block <* symbol "end")
    whileDoStmt = dbg "whiledo stmt" $ uncurry WhileDoStmt <$> (symbol "while" *> expr) >*< (symbol "do" *> block <* symbol "end")
    repeatUntilStmt = dbg "repeatuntil stmt" $ uncurry RepeatUntilStmt <$> (symbol "repeat" *> block) >*< (symbol "until" *> expr)
    conditionalStmt = dbg "conditional stmt" $ do
      _ <- dbg "if" $ symbol "if"
      cond1 <- expr
      _ <- dbg "then (if)" $ symbol "then"
      body1 <- block
      elseifs <- many $ do
        _ <- dbg "elseif" $ symbol "elseif"
        cond <- expr
        _ <- dbg "then (elseif)" $ symbol "then"
        body <- block
        return (cond, body)
      elseBody <- optional $ dbg "else" $ symbol "else" >> dbg "block (else)" block
      _ <- dbg "end (conditional)" $ symbol "end"
      return $ ConditionalStmt ((cond1, body1) : elseifs) (fromMaybe [] elseBody)
    forListStmt = dbg "forlist stmt" do
      name <- try (symbol "for" *> name <* symbol "=")
      exprs <- exprlist1
      _ <- symbol "do"
      body <- block
      _ <- symbol "end"
      return $ ForListStmt name exprs body
    forRangeStmt = dbg "forrange stmt" $ do
      names <- try (symbol "for" *> namelist1 <* symbol "in")
      exprs <- exprlist1
      _ <- symbol "do"
      body <- block
      _ <- symbol "end"
      return $ ForRangeStmt names exprs body
    functionStmt = dbg "function stmt" $ uncurry FunctionStmt <$> (symbol "function" *> funcname) >*< funcbody
    localStmt = dbg "local stmt" $ do
      _ <- symbol "local"
      localFunctionStmt <|> localAssignStmt
    localFunctionStmt =
      dbg "local function stmt" $
        uncurry LocalFunctionStmt <$> (symbol "function" *> name >*< funcbody)
    localAssignStmt = dbg "local assign stmt" $ do
      _ <- notFollowedBy $ symbol "function"
      names <- namelist1
      exprs <- optional $ symbol "=" >> exprlist1
      return $ LocalAssignStmt names (fromMaybe [] exprs)
    returnStmt = dbg "return" $ ReturnStmt <$> (symbol "return" >> exprlist)
    breakStmt = dbg "break" $ BreakStmt <$ symbol "break"

-- function ::= function funcbody
-- funcbody ::= `(´ [parlist1] `)´ block end
-- parlist1 ::= namelist [`,´ `...´]  |  `...´
data Function = Function [Name] Bool [Stmt]
  deriving (Show, Eq)

funcbody :: Parser Function
funcbody = do
  _ <- symbol "("
  params <- namelist
  hasVarargs <-
    fmap isJust $
      optional $
        if null params
          then symbol "..."
          else symbol "," >> symbol "..."
  _ <- symbol ")"
  Function params hasVarargs <$> block <* symbol "end"

-- block ::= chunk
-- chunk ::= {stat [`;´]} [laststat[`;´]]
-- laststat ::= return [explist1]  |  break
block :: Parser [Stmt]
block = dbg "block" $ many (stmt <* optional (symbol ";"))

-- case last stmts of
--   ReturnStmt _ -> return stmts
--   BreakStmt -> return stmts
--   _ -> unexpected (Label $ fromList "Unexpected last statement")

-- exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |
--          function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp
data Expr
  = ValueExpr Value
  | VarArgsExpr
  | VarExpr Var
  | FuncExpr Function
  | FnCallExpr FnCall
  | TableConsExpr TableCons
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  deriving (Show, Eq)

data Value
  = NilValue
  | BoolValue Bool
  | NumValue Double
  | StrValue String
  deriving (Show, Eq)

data BinOp
  = -- arithmetic
    Add
  | Sub
  | Mul
  | Div
  | FloorDiv
  | Mod
  | Exp
  | -- string
    Concat
  | -- relational
    Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Neq
  | -- logical
    And
  | Or
  | -- bitwise
    BitAnd
  | BitOr
  | BitXor
  | BitRightShift
  | BitLeftShift
  deriving (Show, Eq)

data UnOp = Neg | BitNot | Not | Len
  deriving (Show, Eq)

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
  where
    term =
      foldl1
        (<|>)
        [ nilExpr,
          boolExpr,
          numExpr,
          strExpr,
          varArgsExpr,
          functionExpr,
          prefixExpr,
          tableConsExpr
        ]
        <?> "term"

    nilExpr = ValueExpr NilValue <$ symbol "nil"
    boolExpr = ValueExpr . BoolValue <$> boolean
    numExpr = ValueExpr . NumValue <$> number
    strExpr = ValueExpr . StrValue <$> stringLit
    varArgsExpr = VarArgsExpr <$ varargs
    functionExpr = FuncExpr <$> (symbol "function" >> funcbody)
    tableConsExpr = TableConsExpr <$> tablecons

    binary sym binop = InfixL (BinOpExpr binop <$ symbol sym)
    unary sym unop = Prefix (UnOpExpr unop <$ symbol sym)
    binary' sym notFollow binop = InfixL (BinOpExpr binop <$ lexeme (try $ string sym <* notFollowedBy notFollow))
    unary' sym notFollow unop = Prefix (UnOpExpr unop <$ lexeme (try $ string sym <* notFollowedBy notFollow))
    opTable =
      [ [binary "^" Exp],
        [ unary "-" Neg,
          unary "not" Not,
          unary' "~" (char '=') BitNot,
          unary "#" Len
        ],
        [ binary "*" Mul,
          binary' "/" (char '/') Div,
          binary "//" FloorDiv,
          binary "%" Mod
        ],
        [ binary "+" Add,
          binary "-" Sub
        ],
        [binary' ".." (char '.') Concat],
        [ binary "<<" BitRightShift,
          binary ">>" BitLeftShift
        ],
        [binary "&" BitAnd],
        [binary' "~" (char '=') BitXor],
        [binary "|" BitOr],
        [ binary' "<" (oneOf ['=', '<']) Lt,
          binary' ">" (oneOf ['=', '>']) Gt,
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
-- var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name
-- functioncall ::=  prefixexp args  |  prefixexp `:´ Name args
-- args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String
prefixExpr :: Parser Expr
prefixExpr = makeExprParser term opTable <?> "prefix expr"
  where
    term :: Parser Expr
    term =
      VarExpr . VarName
        <$> name
          <|> parens expr
    opTable = [[suffixChain]]
    suffixChain :: Operator Parser Expr
    suffixChain = Postfix $ foldr1 (flip (.)) <$> some (subscript <|> member <|> functioncall)
    subscript = dbg' "subscript suffix" $ do
      s <- squareBrackets expr
      return \prefix -> VarExpr $ VarSubs prefix s
    member = dbg' "member suffix" do
      _ <- try $ char '.' <* notFollowedBy (char '.')
      m <- name
      return \prefix -> VarExpr $ VarMember prefix m
    functioncall = dbg' "functioncall suffix" do
      method <- optional $ symbol ":" >> name
      args <- args
      return \prefix -> FnCallExpr $ FnCall prefix method args
    args =
      parens
        exprlist
        <|> ( singleton
                . TableConsExpr
                <$> tablecons
            )
        <|> ( singleton
                . ValueExpr
                . StrValue
                <$> stringLit
            )

data Var
  = VarName Name
  | VarSubs Expr Expr -- prefixexp[exp]
  | VarMember Expr Name -- prefixexp.name
  deriving (Show, Eq)

var :: Parser Var
var = dbg "var" $ do
  prefixExpr <- prefixExpr
  case prefixExpr of
    VarExpr var -> return var
    _ -> unexpected (Label $ fromList "expression")

data FnCall = FnCall Expr (Maybe Name) [Expr]
  deriving (Show, Eq)

functioncall :: Parser FnCall
functioncall = dbg "functioncall" $ do
  prefixExpr <- prefixExpr
  case prefixExpr of
    FnCallExpr fnCall -> return fnCall
    _ -> unexpected (Label $ fromList "expression")

-- tableconstructor ::= `{´ [fieldlist] `}´
-- fieldlist ::= field {fieldsep field} [fieldsep]
-- fieldsep ::= `,´  |  `;´
-- field ::= `[´ exp `]´ `=´ exp  |  Name `=´ exp  |  exp
newtype TableCons = TableCons [Field]
  deriving (Show, Eq)

data Field
  = FieldAssignExpr Expr Expr
  | FieldAssignName Name Expr
  | FieldExpr Expr
  deriving (Show, Eq)

tablecons :: Parser TableCons
tablecons = curlyBrackets (TableCons <$> sepEndBy field fieldsep) <?> "table constructor"
  where
    fieldsep = lexeme $ satisfy (\c -> c == ',' || c == ';')
    field :: Parser Field
    field = fieldAssignExpr <|> fieldAssignName <|> fieldExpr
    fieldAssignExpr =
      do
        lhs <- squareBrackets expr <* symbol "="
        FieldAssignExpr lhs <$> expr
    fieldAssignName =
      do
        lhs <- try $ name <* symbol "="
        FieldAssignName lhs <$> expr
    fieldExpr = FieldExpr <$> expr
