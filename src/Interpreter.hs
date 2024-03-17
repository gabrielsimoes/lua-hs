{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, readIORef, newIORef, writeIORef, modifyIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Vector (MVector)
import Data.HashTable.IO (CuckooHashTable)
import Data.Vector.Mutable (IOVector)
import Parser (Expr (..), BinOp (..), UnOp (..))
import Control.Monad.Except (throwError, runExceptT, MonadError (catchError))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad (liftM)
import qualified Data.String as T

data Value = Integer Int64
           | Float Double
           | String BS.ByteString
           | Boolean Bool
           | Nil
           -- | TableValue Table
           -- | Function
  deriving (Eq)

instance Show Value where
  show (Integer i) = show i
  show (Float f) = show f
  show (String s) = show s
  show (Boolean b) = show b
  show Nil = "nil"

data Table = Table { array :: TableArray
                   , hash :: TableHash
                   }
type TableArray = IOVector Value
type TableHash = CuckooHashTable TableKey Value
data TableKey = TableKeyInt Int64
              | TableKeyString BS.ByteString
              deriving (Eq, Generic)
instance Hashable TableKey

data Env = Env { parent :: IORef (Maybe Env)
               , values :: IORef (Map String (IORef Value))
               }

emptyEnv :: IO Env
emptyEnv = descend Nothing

descend :: Maybe Env -> IO Env
descend parent = liftA2 Env (newIORef parent) (newIORef Map.empty)

ascend :: Env -> IO (Maybe Env)
ascend env = readIORef (parent env)

get :: Env -> String -> IO (Maybe (IORef Value))
get env name = do
  values <- readIORef (values env)
  case Map.lookup name values of
    Just value -> pure $ Just value
    Nothing -> do
      parent <- readIORef (parent env)
      maybe (pure Nothing) (`get` name) parent

set :: Env -> String -> Value -> IO ()
set env name value = do
  valueRef <- newIORef value
  modifyIORef' (values env) (Map.insert name valueRef)

type EvalError = String
type ThrowsError = Either EvalError
type IOThrowsError = ExceptT EvalError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

trapError action = catchError action (return . ((++) "error: "))

eval :: Env -> Expr -> IOThrowsError Value
eval env expr = case expr of
  NilExpr -> pure Nil
  BoolExpr b -> pure $ Boolean b
  IntExpr i -> pure $ Integer i
  FloatExpr f -> pure $ Float f
  StrExpr s -> pure $ String s
  VarArgsExpr -> throwError "VarArgsExpr not implemented"
  VarExpr v -> throwError "VarExpr not implemented"
  FuncExpr f -> throwError "FuncExpr not implemented"
  FnCallExpr fc -> throwError "FnCallExpr not implemented"
  TableConsExpr tc -> throwError "TableConsExpr not implemented"
  BinOpExpr binOp leftExpr rightExpr -> do
    left <- eval env leftExpr
    right <- eval env rightExpr
    let op = case binOp of
          -- Arithmetic
          Add -> numOp (Just (+)) (+)
          Sub -> numOp (Just (-)) (-)
          Mul -> numOp (Just (*)) (*)
          Div -> numOp Nothing (/)
          FloorDiv -> numOp (Just div) doubleFloorDiv
          Mod -> numOp (Just mod) doubleMod
          Exp -> numOp Nothing (**)
          -- String
          Concat -> (liftThrows .) . concatString
          -- Relational
          Lt -> \left right -> throwError "Lt not implemented"
          Gt -> \left right -> throwError "Gt not implemented"
          Lte -> \left right -> throwError "Lte not implemented"
          Gte -> \left right -> throwError "Gte not implemented"
          Eq -> \left right -> throwError "Eq not implemented"
          Neq -> \left right -> throwError "Neq not implemented"
          -- Logical
          And -> \left right -> throwError "And not implemented"
          Or -> \left right -> throwError "Or not implemented"
          -- Bitwise
          BitAnd -> \left right -> pure $ Boolean $ False
          BitOr -> \left right -> pure $ Boolean $ False
          BitXor -> \left right -> pure $ Boolean $ False
          BitRightShift -> \left right -> pure $ Boolean $ False
          BitLeftShift -> \left right -> pure $ Boolean $ False
          where
            doubleFloorDiv a b = fromIntegral $ div (floor a) (floor b)
            doubleMod a b = a - doubleFloorDiv a b * b

            numOp :: Maybe (Int64 -> Int64 -> Int64) -> (Double -> Double -> Double) -> Value -> Value -> IOThrowsError Value
            numOp maybeIntOp floatOp (Integer left) (Integer right) = pure $ case maybeIntOp of
              Just intOp -> Integer $ intOp left right
              Nothing -> Float $ floatOp (fromIntegral left) (fromIntegral right)
            numOp _ floatOp (Integer left) (Float right) = pure $ Float $ floatOp (fromIntegral left) right
            numOp _ floatOp (Float left) (Integer right) = pure $ Float $ floatOp left (fromIntegral right)
            numOp _ floatOp (Float left) (Float right) = pure $ Float $ floatOp left right
            numOp _ _ _ _ = throwError "numOp: invalid types"

            concatString :: Value -> Value -> ThrowsError Value
            concatString a b = fmap String $ liftA2 BS.append (coerceString a) (coerceString b)
            coerceString :: Value -> ThrowsError BS.ByteString
            coerceString = \case
                  Integer i -> Right $ BS.pack $ show i
                  Float f -> Right $ BS.pack $ show f
                  String s -> Right s
                  Boolean b -> Left "attempt to concatenate a boolean value"
                  Nil -> Left "attempt to concatenate a nil value"
    op left right
  UnOpExpr op e -> do
    value <- eval env e
    case op of
      Neg -> case value of
        Integer i -> pure $ Integer $ -i
        Float f -> pure $ Float $ -f
        _ -> throwError "Neg: invalid type"
      BitNot -> throwError "BitNot not implemented"
      Not -> case value of
        Boolean b -> pure $ Boolean $ not b
        _ -> throwError "Not: invalid type"
      Len -> throwError "Len not implemented"

