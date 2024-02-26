{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (Parser, program, expr, Expr (..), UnOp (..), functioncall, exprlist)
import Text.Megaparsec (parseTest, MonadParsec (eof, takeWhileP), try, runParser, errorBundlePretty, skipManyTill, tokens, anySingle, optional, skipMany, anySingleBut, some)
import Text.Printf (printf)
import Text.Megaparsec.Debug (dbg)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Token (sc, symbol, number, stringLit)
import Data.Either (isRight)
import System.Directory (getDirectoryContents, listDirectory, doesFileExist, doesDirectoryExist)
import Control.Monad (void, filterM, foldM, unless, when)
import System.FilePath ((</>), takeExtension)
import System.Directory.Internal.Prelude (exitFailure)
import Text.Megaparsec.Char (string, char)
import qualified GHC.IO.Device as T

skipHash :: Parser a -> Parser a
skipHash p = optional (char '#' *> skipMany (anySingleBut '\n')) *> p

testParse :: Show a => Parser a -> String -> T.Text -> Bool -> IO Bool
testParse parser name text verbose = do
  let result = runParser parser name text
  case result of
    Left err -> putStr $ errorBundlePretty err
    Right result -> do
      putStrLn (name ++ ": OK")
      when verbose (print result)
  return $ isRight result

testParseFile :: FilePath -> IO Bool
testParseFile file = do
  content <- T.readFile file
  testParse (skipHash program) file content False

allFilesInRecursive :: FilePath -> IO [FilePath]
allFilesInRecursive dir = do
  children <- listDirectory dir
  concat <$> mapM (recurse dir) children
  where
    recurse :: FilePath -> FilePath -> IO [FilePath]
    recurse parent child = do
      let path = parent </> child
      isDir <- doesDirectoryExist path
      if isDir
        then allFilesInRecursive path
        else pure [path]

main :: IO ()
main = do
  -- ok <- testParse expr "e1" "-2^-2" True
  -- ok <- testParse expr "e2" "-2^- - -2" True
  -- ok <- testParse expr "e3" "2^2^3" True
  -- results <- mapM (\ (n, s) -> testParse (number <* eof) n s True) [
  --     ("i1", "3"),
  --     ("i2", "345"),
  --     ("i3", "0xff"),
  --     ("i4", "0xBEBADA"),
  --     ("i5", "0"),
  --     ("f1", "3.0"),
  --     ("f2", "3.1416"),
  --     ("f3", "314.16e-2"),
  --     ("f4", "0.31416E1"),
  --     ("f5", "34e1"),
  --     ("f6", "0x0.1E"),
  --     ("f7", "0xA23p-4"),
  --     ("f8", "0X1.921FB54442D18P+1"),
  --     ("f9", ".4"),
  --     ("f10", "0x0p12"),
  --     ("f11", "0x.0p-3")
  --   ]
  -- ok <- testParse expr "f" "~~-1024.0" True
  -- ok <- testParse expr "s" "'a\\z b'" True
  -- ok <- testParse exprlist "s" "'testing scanner'" True
  -- ok <- testParse functioncall "f" "print('testing scanner')" True
  -- ok <- testParseFile "test/lua-5.4.6-tests/literals.lua"
  luaFiles <- fmap (filter ((== ".lua") . takeExtension)) (allFilesInRecursive "test/")
  results <- mapM testParseFile luaFiles
  let ok = foldl (||) True results
  unless ok exitFailure
