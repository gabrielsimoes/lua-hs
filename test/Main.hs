{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, foldM, unless, void, when)
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GHC.IO.Device as T
import Parser (Expr (..), Parser, UnOp (..), expr, exprlist, functioncall, program)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, listDirectory)
import System.Directory.Internal.Prelude (exitFailure)
import System.FilePath (takeExtension, (</>))
import Text.Megaparsec (MonadParsec (eof, takeWhileP), anySingle, anySingleBut, errorBundlePretty, optional, parseTest, runParser, skipMany, skipManyTill, some, tokens, try)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Debug (dbg)
import Text.Printf (printf)
import Token (number, sc, stringLit, symbol)
import Interpreter (evalRepr, emptyEnv, evalRepr, runIOThrows)
import System.Process (readProcess, readProcessWithExitCode)
import Values (repr)

skipHash :: Parser a -> Parser a
skipHash p = optional (char '#' *> skipMany (anySingleBut '\n')) *> p

testParse :: (Show a) => Parser a -> String -> T.Text -> Bool -> IO Bool
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

testParseAllFiles :: IO Bool
testParseAllFiles = do
  luaFiles <- fmap (filter ((== ".lua") . takeExtension)) (allFilesInRecursive "test/")
  results <- mapM testParseFile luaFiles
  return $ foldl (||) True results

testExponentiation :: IO Bool
testExponentiation = do
  results <-
    sequence
      [ testParse expr "e1" "-2^-2" True,
        testParse expr "e2" "-2^- - -2" True,
        testParse expr "e3" "2^2^3" True
      ]
  return $ foldl (||) True results

testParseNumericalLiterals :: IO Bool
testParseNumericalLiterals = do
  results <-
    mapM
      (\(n, s) -> testParse (number <* eof) n s True)
      [ ("i1", "3"),
        ("i2", "345"),
        ("i3", "0xff"),
        ("i4", "0xBEBADA"),
        ("i5", "0"),
        ("f1", "3.0"),
        ("f2", "3.1416"),
        ("f3", "314.16e-2"),
        ("f4", "0.31416E1"),
        ("f5", "34e1"),
        ("f6", "0x0.1E"),
        ("f7", "0xA23p-4"),
        ("f8", "0X1.921FB54442D18P+1"),
        ("f9", ".4"),
        ("f10", "0x0p12"),
        ("f11", "0x.0p-3")
      ]
  return $ foldl (||) True results

testParseWeirdStuff :: IO Bool
testParseWeirdStuff = do
  results <-
    sequence
      [ testParse expr "f" "~~-1024.0" True,
        testParse expr "s" "'a\\z b'" True,
        testParse exprlist "s" "'testing scanner'" True,
        testParse functioncall "f" "print('testing scanner')" True
      ]
  return $ foldl (||) True results

runParserAndEval :: T.Text -> IO Bool
runParserAndEval text = do
  let parseResult = runParser expr (T.unpack text) text
  case parseResult of
    Left err -> do
      putStrLn $ T.unpack text ++ " " ++ (errorBundlePretty err)
      return False
    Right expr -> do
      env <- emptyEnv
      result <- runIOThrows $ evalRepr env expr
      (luaExit, luaStdout, luaStderr) <- readProcessWithExitCode "lua" ["-e", "print(" ++ T.unpack text ++ ")"] ""
      putStrLn $ T.unpack text ++ "=\n" ++ result ++ "\n" ++ (firstLine (luaStdout ++ luaStderr))
      return True
  where
    firstLine s = takeWhile (/= '\n') s


main :: IO ()
main = do
  -- ok <- testParseFile "test/lua-5.4.6-tests/literals.lua"
  -- ok <- testParseAllFiles
  -- unless ok exitFailure
  env <- emptyEnv
  _ <- runParserAndEval "2 + 2 + 2"
  _ <- runParserAndEval "(2 * 2) // 2"
  _ <- runParserAndEval "(2 * 2) / 2"
  _ <- runParserAndEval "5.1 // 0.19"
  _ <- runParserAndEval "5 // 0.19"
  _ <- runParserAndEval "-5 / 0.0"
  _ <- runParserAndEval "-5 // 0.0"
  _ <- runParserAndEval "-5 / 0"
  _ <- runParserAndEval "5 / 0"
  _ <- runParserAndEval "0 / 0"
  _ <- runParserAndEval "-(0 / 0)"
  _ <- runParserAndEval "-5 % 2"
  _ <- runParserAndEval "-5 // 0"
  _ <- runParserAndEval "-5.0 % 2"
  _ <- runParserAndEval "2 ^ 3"
  _ <- runParserAndEval "'concat: ' .. 1 .. ' ' .. 1.0"
  _ <- runParserAndEval "'concat: ' .. nil"
  _ <- runParserAndEval "'concat: ' .. true"
  _ <- runParserAndEval "'concat: ' .. {}"
  _ <- runParserAndEval "{}"
  _ <- runParserAndEval "{1, ['some' .. '-' .. 'key'] = 'value', 2, foo = 'bar', [3] = 3, [2 * 2 * 2] = 8}"
  return ()

