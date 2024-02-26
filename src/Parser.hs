{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators (some)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Postfix, Prefix), makeExprParser)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( ErrorItem (Label),
    Parsec,
    between,
    choice,
    eof,
    many,
    notFollowedBy,
    oneOf,
    option,
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
import qualified Text.Megaparsec.Debug as D (dbg, dbg')
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
import Data.ByteString.Char8 (ByteString)

-- parse over text tokens
type Parser = Parsec Void Text

program :: Parser [Stat]
program = sc >> block <* eof

-- name
newtype Name = Name String
  deriving (Show, Eq)

name :: Parser Name
name = Name <$> identifier

-- funcname ::= Name {‘.’ Name} [‘:’ Name]
data FuncName = FuncName (NonEmpty Name) (Maybe Name)
  deriving (Show, Eq)

funcname :: Parser FuncName
funcname = do
  names <- sepBy1 name (symbol ".")
  method <- optional $ symbol ":" >> name
  return $ FuncName (fromList names) method

-- attnamelist ::=  Name attrib {‘,’ Name attrib}
-- attrib ::= [‘<’ Name ‘>’]
data AttName = AttName Name (Maybe Name)
  deriving (Show, Eq)

attname :: Parser AttName
attname = liftA2 AttName name (optional $ between (symbol "<") (symbol ">") name)

attnamelist1 :: Parser (NonEmpty AttName)
attnamelist1 = fromList <$> sepBy1 attname (symbol ",")

-- namelist ::= Name {‘,’ Name}
namelist :: Parser [Name]
namelist = sepBy name (try $ symbol "," <* notFollowedBy (symbol "...")) <?> "namelist"

namelist1 :: Parser (NonEmpty Name)
namelist1 = fromList <$> sepBy1 name (symbol ",") <?> "namelist1"

-- varlist ::= var {‘,’ var}
varlist1 :: Parser (NonEmpty Var)
varlist1 = fromList <$> sepBy1 var (symbol ",") <?> "varlist1"

-- explist ::= exp {‘,’ exp}
exprlist :: Parser [Expr]
exprlist = sepBy expr (symbol ",") <?> "exprlist"

exprlist1 :: Parser (NonEmpty Expr)
exprlist1 = fromList <$> sepBy1 expr (symbol ",") <?> "exprlist1"

-- stat ::=  ‘;’ |
--           varlist ‘=’ explist |
--           functioncall |
--           label |
--           break |
--           goto Name |
--           do block end |
--           while exp do block end |
--           repeat block until exp |
--           if exp then block {elseif exp then block} [else block] end |
--           for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
--           for namelist in explist do block end |
--           function funcname funcbody |
--           local function Name funcbody |
--           local attnamelist [‘=’ explist]
-- label ::= ‘::’ Name ‘::’
data Stat
  = AssignStat (NonEmpty Var) (NonEmpty Expr)
  | FnCallStat FnCall
  | BlockStat [Stat]
  | WhileDoStat Expr [Stat]
  | RepeatUntilStat [Stat] Expr
  | ConditionalStat [(Expr, [Stat])] [Stat]
  | ForListStat Name (NonEmpty Expr) [Stat]
  | ForRangeStat (NonEmpty Name) (NonEmpty Expr) [Stat]
  | FunctionStat FuncName Function
  | LocalFunctionStat Name Function
  | LocalAssignStat (NonEmpty AttName) [Expr]
  | ReturnStat [Expr]
  | BreakStat
  | LabelStat Name
  | GotoStat Name
  deriving (Show, Eq)

-- isDebugEnabled = True
isDebugEnabled = False

dbg :: (Show a) => String -> Parser a -> Parser a
dbg = if isDebugEnabled then D.dbg else const id

dbg' :: String -> Parser a -> Parser a
dbg' = if isDebugEnabled then D.dbg' else const id

stat :: Parser Stat
stat =
  dbg
    "stat"
    ( choice
        [ labelStat,
          gotoStat,
          whileDoStat,
          conditionalStat,
          try assignStat,
          try fnCallStat,
          blockStat,
          repeatUntilStat,
          forListStat,
          forRangeStat,
          functionStat,
          localStat,
          returnStat,
          breakStat
        ]
        <?> "statement"
    )
  where
    assignStat = dbg "assign stat" $ do
      vars <- varlist1
      _ <- symbol "="
      AssignStat vars <$> exprlist1
    fnCallStat = dbg "fncall stat" $ FnCallStat <$> functioncall
    blockStat = dbg "block stat" $ BlockStat <$> (symbol "do" *> block <* symbol "end")
    whileDoStat =
      dbg "whiledo stmt" $
        liftA2 WhileDoStat (symbol "while" *> expr) (symbol "do" *> block <* symbol "end")
    repeatUntilStat =
      dbg "repeatuntil stmt" $
        liftA2 RepeatUntilStat (symbol "repeat" *> block) (symbol "until" *> expr)
    conditionalStat = dbg "conditional stmt" $ do
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
      elseBody <- option [] $ dbg "else" $ symbol "else" >> dbg "block (else)" block
      _ <- dbg "end (conditional)" $ symbol "end"
      return $ ConditionalStat ((cond1, body1) : elseifs) elseBody
    forListStat = dbg "forlist stmt" do
      name <- try (symbol "for" *> name <* symbol "=")
      exprs <- exprlist1
      _ <- symbol "do"
      body <- block
      _ <- symbol "end"
      return $ ForListStat name exprs body
    forRangeStat = dbg "forrange stmt" $ do
      names <- try (symbol "for" *> namelist1 <* symbol "in")
      exprs <- exprlist1
      _ <- symbol "do"
      body <- block
      _ <- symbol "end"
      return $ ForRangeStat names exprs body
    functionStat =
      dbg "function stmt" $
        liftA2 FunctionStat (symbol "function" *> funcname) funcbody
    localStat = dbg "local stmt" $ do
      _ <- symbol "local"
      localFunctionStat <|> localAssignStat
    localFunctionStat =
      dbg "local function stmt" $
        liftA2 LocalFunctionStat (symbol "function" *> name) funcbody
    localAssignStat = dbg "local assign stmt" $ do
      _ <- notFollowedBy $ symbol "function"
      attnames <- attnamelist1
      exprs <- optional $ symbol "=" >> exprlist1
      return $ LocalAssignStat attnames (maybe [] toList exprs)
    returnStat = dbg "return" $ ReturnStat <$> (symbol "return" >> exprlist)
    breakStat = dbg "break" $ BreakStat <$ symbol "break"
    labelStat = dbg "label" $ LabelStat <$> (symbol "::" *> name <* symbol "::")
    gotoStat = dbg "goto" $ GotoStat <$> (symbol "goto" *> name)

-- label ::= ‘::’ Name ‘::’

-- function ::= function funcbody
-- funcbody ::= `(´ [parlist1] `)´ block end
-- parlist1 ::= namelist [`,´ `...´]  |  `...´
data Function = Function [Name] Bool [Stat]
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

-- chunk ::= block
-- block ::= {stat} [retstat]
-- retstat ::= return [explist] [‘;’]
block :: Parser [Stat]
block = dbg "block" $ seps *> many (stat <* seps)
  where
    seps = many (symbol ";")

-- exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |
--          function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp
data Expr
  = NilExpr
  | BoolExpr Bool
  | IntExpr Int64
  | FloatExpr Double
  | StrExpr ByteString
  | VarArgsExpr
  | VarExpr Var
  | FuncExpr Function
  | FnCallExpr FnCall
  | TableConsExpr TableCons
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
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

-- exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
--          prefixexp | tableconstructor | exp binop exp | unop exp
expr :: Parser Expr
expr = infixTerm
  where
    infixTerm = makeExprParser prefixTerm infixOpTable <?> "expression"

    -- handle repeated unary operators and exponentiation precendence
    prefixTerm = do
      prefixOps <- many $ choice prefixOpList
      argument <- expoTerm
      return $ foldr ($) argument prefixOps

    -- exponentiation '^', right-associative
    expoTerm = do 
      lhs <- term
      option lhs (BinOpExpr Exp lhs <$> (symbol "^" >> prefixTerm))

    -- remaining expression types
    term =
      choice
        [ nilExpr,
          boolExpr,
          numExpr,
          strExpr,
          varArgsExpr,
          functionExpr,
          prefixExpr,
          tableConstructorExpr
        ]
        <?> "term"
    nilExpr = NilExpr <$ symbol "nil"
    boolExpr = BoolExpr <$> boolean
    numExpr = either IntExpr FloatExpr <$> number
    strExpr = StrExpr <$> stringLit
    varArgsExpr = VarArgsExpr <$ varargs
    functionExpr = FuncExpr <$> (symbol "function" >> funcbody)
    tableConstructorExpr = TableConsExpr <$> tableconstructor

    -- unop ::= ‘-’ | not | ‘#’ | ‘~’
    -- binop ::= ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
    --           ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | and | or
    prefixOpList :: [Parser (Expr -> Expr)]
    prefixOpList =
      [ unary' "-" "-" Neg,
        unary "not" Not,
        unary' "~" (char '=') BitNot,
        unary "#" Len
      ]
    infixOpTable :: [[Operator Parser Expr]]
    infixOpTable =
      [ [ infixL "*" Mul,
          infixL' "/" (char '/') Div,
          infixL "//" FloorDiv,
          infixL "%" Mod
        ],
        [ infixL "+" Add,
          infixL "-" Sub
        ],
        [infixR' ".." (char '.') Concat],
        [ infixL "<<" BitRightShift,
          infixL ">>" BitLeftShift
        ],
        [infixL "&" BitAnd],
        [infixL' "~" (char '=') BitXor],
        [infixL "|" BitOr],
        [ infixL' "<" (oneOf ['=', '<']) Lt,
          infixL' ">" (oneOf ['=', '>']) Gt,
          infixL "<=" Lte,
          infixL ">=" Gte,
          infixL "~=" Neq,
          infixL "==" Eq
        ],
        [ infixL "and" And
        ],
        [infixL "or" Or]
      ]

    unary sym unop =
      -- (dbg' $ "unary " ++ T.unpack sym)
        (UnOpExpr unop <$ symbol sym)
    unary' sym notFollow unop =
      -- (dbg' $ "unary' " ++ T.unpack sym)
        (UnOpExpr unop <$ (lexeme . try) (string sym <* notFollowedBy notFollow))

    infixL sym binop =
      InfixL $
        -- (dbg' $ "infixL " ++ T.unpack sym)
          (BinOpExpr binop <$ symbol sym)
    infixR sym binop =
      InfixR $
        -- (dbg' $ "infixR " ++ T.unpack sym)
          (BinOpExpr binop <$ symbol sym)
    infixL' sym notFollow binop =
      InfixL $
        -- (dbg' $ "infixL' " ++ T.unpack sym)
          (BinOpExpr binop <$ (lexeme . try) (string sym <* notFollowedBy notFollow))
    infixR' sym notFollow binop =
      InfixL $
        -- (dbg' $ "infixR' " ++ T.unpack sym)
          (BinOpExpr binop <$ (lexeme . try) (string sym <* notFollowedBy notFollow))

-- prefixexp ::= var  |  functioncall  |  `(´ exp `)´
-- var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name
-- functioncall ::=  prefixexp args  |  prefixexp `:´ Name args
-- args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String
prefixExpr :: Parser Expr
prefixExpr = dbg "prefixexpr" (makeExprParser term opTable <?> "prefix expr")
  where
    term :: Parser Expr
    term =
      dbg "prefixexpr term" $
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
      _ <- try $ symbol "." <* notFollowedBy (symbol ".")
      m <- name
      return \prefix -> VarExpr $ VarMember prefix m
    functioncall = dbg' "functioncall suffix" do
      method <- try $ optional $ symbol ":" >> name
      args <- args
      return \prefix -> FnCallExpr $ FnCall prefix method args
    args =
      parens exprlist
        <|> (singleton . TableConsExpr <$> tableconstructor)
        <|> (singleton . StrExpr <$> stringLit)

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

newtype TableCons = TableCons [Field]
  deriving (Show, Eq)

data Field
  = FieldAssignExpr Expr Expr -- [expr] = expr
  | FieldAssignName Name Expr -- name = expr
  | FieldExpr Expr -- expr
  deriving (Show, Eq)

-- tableconstructor ::= ‘{’ [fieldlist] ‘}’
-- fieldlist ::= field {fieldsep field} [fieldsep]
tableconstructor :: Parser TableCons
tableconstructor = curlyBrackets (TableCons <$> sepEndBy field fieldsep) <?> "table constructor"
  where
    -- fieldsep ::= ‘,’ | ‘;’
    fieldsep = lexeme $ satisfy (\c -> c == ',' || c == ';')

    -- field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
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
