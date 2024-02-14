{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)
import Token (expr)
import Text.Megaparsec (parseTest, MonadParsec (eof))
import Text.Printf (printf)

-- tokenizeFile :: FilePath -> IO ()
-- tokenizeFile filePath = do
--   file <- openFile filePath ReadMode
--   contents <- hGetContents file
--   let tokens = tokenize contents
--   print tokens
--   hClose file

main :: IO ()
main = do
  parseTest (expr <* eof) "1 + 2.2 + (0.4 + 4.57e-3) + 0.3e12 + 5e+20 .. \"abc\" .. 'def'"
  -- tokenizeFile "test/resources/learnlua.lua"
  printf "OK"
