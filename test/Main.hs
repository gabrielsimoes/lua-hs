{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)
import Parser (expr, block, stmt, Parser, program)
import Text.Megaparsec (parseTest, MonadParsec (eof), try)
import Text.RawString.QQ
import Text.Printf (printf)
import Data.Text (pack)
import Text.Megaparsec.Debug (dbg)
import qualified Data.Text as T
import Token (sc)

parseFile parser filepath = do
   file <- openFile filepath ReadMode
   contents <- hGetContents file
   parseTest (try sc >> parser) $ T.pack contents
   hClose file

test :: String
test = [r|ab[a][b][c]:b(a[c],b, c)
--[=====[
1 + 2.2 + 
--[[
--
-- this should be ignored
--]]
--[=[
--]=]
--[==[
    [ this should be ignored ]
    [[ this should be ignored ]]
    [=[ this should be ignored ]=]
--]==]
---[[ not a block comment
(0.4 + 4.57e-3) + 0.3e12 + 
-- this should be ignored
5e+20 .. "abcdef" .. 'def' .. [=[
<HTML>
<HEAD>
<TITLE>An HTML Page</TITLE>
</HEAD>
<BODY>
 <A HREF="http://www.lua.org">Lua</A>
 [[a text between double brackets]]
</BODY>
</HTML>
]=]
]=====]
|]

test2 = [r|

if num > 40 then
  print('over 40')
elseif s ~= 'walternate' then  -- ~= is not equals.
  -- Equality check is == like Python; ok for strs.
  io.write('not over 40\n')  -- Defaults to stdout.
else
  -- Variables are global by default.
  thisIsGlobal = 5  -- Camel case is common.

  -- How to make a variable local:
  local line = io.read()  -- Reads next stdin line.

  -- String concatenation uses the .. operator:
  print('Winter is coming, ' .. line)
end

x,y,z = 2,3
y = 3

num = 42  -- Numbers can be integer or floating point.

s = 'walternate'  -- Immutable strings like Python.
t = "double-quotes are also fine"
u = [[ Double brackets
       start and end
       multi-line strings.]]
t = nil  -- Undefines t; Lua has garbage collection.

-- Blocks are denoted with keywords like do/end:
while num < 50 do
  num = num + 1  -- No ++ or += type operators.
end
|]

main :: IO ()
main = do
  -- printf test
  -- printf "\n"
  -- parseTest (expr <* eof) $ pack test
  -- parseTest (program <* eof) $ pack test2
  parseFile (dbg "dbg" (block <* eof)) "test/resources/learnlua.lua"
  -- printf "OK"

