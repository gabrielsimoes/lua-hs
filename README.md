# lua-hs

A parser and interpreter for Lua, written in Haskell.

The goal of this project is to implement [Lua 5.4](https://www.lua.org/manual/5.4/).

This is not intended to be production-level, only a personal exercise in writing parsers and
interpreters using Haskell.

## Grammar and Parsing

Parsing is done using [megaparsec](https://hackage.haskell.org/package/megaparsec).
Lexing is done during parsing using megaparsec's

```
chunk ::= block
block ::= {stat} [retstat]
stat ::=  ‘;’ | 
          varlist ‘=’ explist | 
          functioncall | 
          label | 
          break | 
          goto Name | 
          do block end | 
          while exp do block end | 
          repeat block until exp | 
          if exp then block {elseif exp then block} [else block] end | 
          for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
          for namelist in explist do block end | 
          function funcname funcbody | 
          local function Name funcbody | 
          local attnamelist [‘=’ explist] 
attnamelist ::=  Name attrib {‘,’ Name attrib}
attrib ::= [‘<’ Name ‘>’]
retstat ::= return [explist] [‘;’]
label ::= ‘::’ Name ‘::’
funcname ::= Name {‘.’ Name} [‘:’ Name]
varlist ::= var {‘,’ var}
var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
namelist ::= Name {‘,’ Name}
explist ::= exp {‘,’ exp}
exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
         prefixexp | tableconstructor | exp binop exp | unop exp 
prefixexp ::= var | functioncall | ‘(’ exp ‘)’
functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 
args ::= ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
functiondef ::= function funcbody
funcbody ::= ‘(’ [parlist] ‘)’ block end
parlist ::= namelist [‘,’ ‘...’] | ‘...’
tableconstructor ::= ‘{’ [fieldlist] ‘}’
fieldlist ::= field {fieldsep field} [fieldsep]
field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
fieldsep ::= ‘,’ | ‘;’
binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
           ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
           ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
           and | or
unop ::= ‘-’ | not | ‘#’ | ‘~’
```

Source: https://www.lua.org/manual/5.4/manual.html#9

Worth mentioning how Lua strings literals and comments behave (not included in the grammar above).
Single line string literals can be delimited by single our double quotes:

```lua
'a string'
"another string"
```

Block literals are delimitedby two square brackets: `[[` and `]]` with any number of equal signs
in between the brackets:

```lua
[==[
a multiline string
]==]
```

Block comments behave similarly: 

```lua
-- this is a line comment
--- this is still a line comment
--[[ this is
a block comment]]
--[===[
    this is also a [[ block comment ]]
]===]
```

These distinctions are made in the lexing helpers.

Parsing of `functioncall` and `varlist`, when used outside an expression, is done by parsing an
arbitrary prefixexp and backtracking in case of failures, and asserting that it is an expression
of the expected type (`functioncall` or `varlist`)

## Interpreter

TBD

## References

- Megaparsec: https://hackage.haskell.org/package/megaparsec
- Megaparsec tutorial: https://markkarpov.com/tutorial/megaparsec.html
- Lua source code: https://www.lua.org/source/5.4/
