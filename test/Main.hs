{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

-- import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)
import Token (expr)
import Text.Megaparsec (parseTest, MonadParsec (eof))
import Text.RawString.QQ
import Text.Printf (printf)
import Data.Text (pack)

-- tokenizeFile :: FilePath -> IO ()
-- tokenizeFile filePath = do
--   file <- openFile filePath ReadMode
--   contents <- hGetContents file
--   let tokens = tokenize contents
--   print tokens
--   hClose file

test :: String
test = [r|1 + 2.2 + 
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
|]

main :: IO ()
main = do
  parseTest (expr <* eof) $ pack test
  -- tokenizeFile "test/resources/learnlua.lua"
  printf "OK"
