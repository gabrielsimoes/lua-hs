{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (Value (..), Env, emptyEnv, eval, evalRepr, IOThrowsError, ThrowsError, runIOThrows) where

import Control.Exception (throw)
import Control.Monad (liftM)
import Control.Monad.Except (MonadError (catchError), runExceptT, throwError)
import Control.Monad.RWS (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.State (StateT (runStateT), get, modify)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.String as T
import Debug.Trace (trace)
import GHC.Float (isDoubleNaN)
import GHC.Generics (Generic)
import Parser (ArithBinOp (..), BinOp (..), BitwiseBinOp (..), Expr (..), Field (..), LogicalBinOp (..), Name (..), TableCons (..), UnOp (..))
import Values (TableKey (..), Value (..), emptyTable, tableLen, tableSet, repr)

data Env = Env
  { parent :: IORef (Maybe Env),
    values :: IORef (Map String (IORef Value))
  }

emptyEnv :: IO Env
emptyEnv = descend Nothing

descend :: Maybe Env -> IO Env
descend parent = liftA2 Env (newIORef parent) (newIORef Map.empty)

ascend :: Env -> IO (Maybe Env)
ascend env = readIORef (parent env)

envGet :: Env -> String -> IO (Maybe (IORef Value))
envGet env name = do
  values <- readIORef (values env)
  case Map.lookup name values of
    Just value -> pure $ Just value
    Nothing -> do
      parent <- readIORef (parent env)
      maybe (pure Nothing) (`envGet` name) parent

envSet :: Env -> String -> Value -> IO ()
envSet env name value = do
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

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . ((++) "error: "))

evalRepr :: Env -> Expr -> IOThrowsError String
evalRepr env expr = do
  value <- eval env expr
  liftIO $ repr value

-- expressions
eval :: Env -> Expr -> IOThrowsError Value
--
-- value expressions
eval env (NilExpr) = pure Nil
eval env (BoolExpr b) = pure $ Boolean b
eval env (IntExpr i) = pure $ Integer i
eval env (FloatExpr f) = pure $ Float f
eval env (StrExpr s) = pure $ String s
eval env (VarArgsExpr) = throwError "VarArgsExpr not implemented"
eval env (VarExpr v) = throwError "VarExpr not implemented"
eval env (FuncExpr f) = throwError "FuncExpr not implemented"
eval env (FnCallExpr fc) = throwError "FnCallExpr not implemented"
--
-- table constructors
eval env (TableConsExpr (TableCons fields)) = do
  table <- liftIO emptyTable
  keyValuePairs <- fst <$> runStateT (mapM evalField fields) 0
  liftIO $ mapM_ (uncurry $ tableSet table) keyValuePairs
  pure $ Table table
  where
    evalField :: Field -> StateT Int64 IOThrowsError (TableKey, Value)
    evalField (FieldAssignExpr keyExpr valueExpr) = do
      keyVal <- lift $ eval env keyExpr
      key <- lift $ liftThrows $ coerceTableKey keyVal
      value <- lift $ eval env valueExpr
      pure (key, value)
    evalField (FieldAssignName (Name name) valueExpr) = do
      let key = TableKeyString $ BS.pack name
      value <- lift $ eval env valueExpr
      pure (key, value)
    evalField (FieldExpr valueExpr) = do
      _ <- modify (+ 1)
      key <- TableKeyInt <$> get
      value <- lift $ eval env valueExpr
      pure (key, value)

    coerceTableKey :: Value -> ThrowsError TableKey
    coerceTableKey = \case
      Integer i -> pure $ TableKeyInt i
      String s -> pure $ TableKeyString s
      _ -> Left $ "attempt to use a non-integer or non-string value as a table key"
--
-- arithmetic expressions
eval env (BinOpExpr (ArithBinOp binOp) leftExpr rightExpr) = do
  left <- eval env leftExpr
  right <- eval env rightExpr
  let op = case binOp of
        Add -> arithmeticOp (Just $ totalOp (+)) (totalOp (+))
        Sub -> arithmeticOp (Just $ totalOp (-)) (totalOp (-))
        Mul -> arithmeticOp (Just $ totalOp (*)) (totalOp (*))
        Div -> arithmeticOp Nothing (totalOp (/))
        FloorDiv -> arithmeticOp (Just $ integerFloorDiv) (totalOp doubleFloorDiv)
        Mod -> arithmeticOp (Just $ totalOp mod) (totalOp doubleMod)
        Pow -> arithmeticOp Nothing (totalOp (**))
  liftThrows $ op left right
  where
    totalOp op = (pure .) . op
    arithmeticOp ::
      Maybe (Int64 -> Int64 -> ThrowsError Int64) ->
      (Double -> Double -> ThrowsError Double) ->
      Value ->
      Value ->
      ThrowsError Value
    arithmeticOp maybeIntOp floatOp leftValue rightValue = case (leftValue, rightValue) of
      (Integer left, Integer right) -> case maybeIntOp of
        Just intOp -> Integer <$> intOp left right
        Nothing -> Float <$> floatOp (fromIntegral left) (fromIntegral right)
      (Integer left, Float right) -> Float <$> floatOp (fromIntegral left) right
      (Float left, Integer right) -> Float <$> floatOp left (fromIntegral right)
      (Float left, Float right) -> Float <$> floatOp left right
      (_, _) -> throwError "cannot perform arithmetic operation on non-number value"

    doubleFloorDiv a b = fromIntegral $ floor $ (/) (fromIntegral $ floor a) b
    integerFloorDiv a b =
      if b == 0
        then Left "attempt to divide by zero"
        else pure $ a `div` b
    doubleMod a b = a - doubleFloorDiv a b * b
eval env (UnOpExpr Neg expr) = (eval env expr) >>= liftThrows . arithmeticNegationOp
  where
    arithmeticNegationOp = \case
      Integer i -> pure $ Integer $ -i
      Float f -> pure $ Float $ -f
      _ -> Left "attempt to perform arithmetic on a non-number value"
--
-- string concat and length
eval env (BinOpExpr ConcatBinOp leftExpr rightExpr) = do
  left <- eval env leftExpr
  right <- eval env rightExpr
  liftThrows $ concatString left right
  where
    concatString :: Value -> Value -> ThrowsError Value
    concatString a b = fmap String $ liftA2 BS.append (coerceString a) (coerceString b)

    coerceString :: Value -> ThrowsError BS.ByteString
    coerceString = \case
      Integer i -> Right $ BS.pack $ show i
      Float f -> Right $ BS.pack $ show f
      String s -> Right s
      Boolean b -> Left "attempt to concatenate a boolean value"
      Nil -> Left "attempt to concatenate a nil value"
      Table t -> Left "attempt to concatenate a table value"
eval env (UnOpExpr Len expr) = do
  value <- eval env expr
  case value of
    String s -> pure $ Integer $ fromIntegral $ BS.length s
    Table t -> Integer . fromIntegral <$> (liftIO $ tableLen t)
    Integer _ -> throwError "attempt to get length of a number value"
    Float _ -> throwError "attempt to get length of a number value"
    Boolean _ -> throwError "attempt to get length of a boolean value"
    Nil -> throwError "attempt to get length of a nil value"
-- relational operations
--         -- Relational
--         Lt -> \left right -> throwError "Lt not implemented"
--         Gt -> \left right -> throwError "Gt not implemented"
--         Lte -> \left right -> throwError "Lte not implemented"
--         Gte -> \left right -> throwError "Gte not implemented"
--         Eq -> \left right -> throwError "Eq not implemented"
--         Neq -> \left right -> throwError "Neq not implemented"
-- logical operations
eval env (BinOpExpr (LogicalBinOp binOp) leftExpr rightExpr) = do
  left <- eval env leftExpr
  case binOp of
    And ->
      if (coerceBoolean left)
        then eval env rightExpr
        else pure left
    Or ->
      if (coerceBoolean left)
        then pure left
        else eval env rightExpr
-- bitwise operations
eval env (BinOpExpr (BitwiseBinOp binOp) leftExpr rightExpr) = do
  left <- eval env leftExpr
  right <- eval env rightExpr
  let op = case binOp of
        BitAnd -> bitwiseOp (.&.)
        BitOr -> bitwiseOp (.|.)
        BitXor -> bitwiseOp xor
        BitRightShift -> bitwiseOp (\a b -> shiftR a (fromIntegral b))
        BitLeftShift -> bitwiseOp (\a b -> shiftL a (fromIntegral b))
  liftThrows $ op left right
  where
    bitwiseOp :: (Int64 -> Int64 -> Int64) -> Value -> Value -> ThrowsError Value
    bitwiseOp op left right = Integer <$> liftA2 op (coerceBitwise left) (coerceBitwise right)
eval env (UnOpExpr BitNot expr) = do
  value <- eval env expr
  liftThrows $ Integer . complement <$> coerceBitwise value
-- logical not
eval env (UnOpExpr Not expr) = do
  value <- eval env expr
  pure $ Boolean $ not $ coerceBoolean value

--
coerceBoolean :: Value -> Bool
coerceBoolean = \case
  Boolean b -> b
  Nil -> False
  _ -> True

coerceBitwise :: Value -> ThrowsError Int64
coerceBitwise = \case
  Integer i -> Right i
  Float f ->
    if f == fromIntegral (round f)
      then Right (round f)
      else Left "number has no integer representation"
  _ -> Left "attempt to perform bitwise operation on a non-integer value"
