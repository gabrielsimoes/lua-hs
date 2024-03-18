{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Values (Value(..), repr, Table', emptyTable, tableSet, tableLen, tableGet, TableKey(..)) where

import Control.Applicative (liftA3)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)
import Parser (TableCons)
import qualified Data.Vector as V
import GHC.IO (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)

data Value
  = Integer Int64
  | Float Double
  | String BS.ByteString
  | Boolean Bool
  | Nil
  | Table Table'

repr :: Value -> IO String
repr (Integer i) = pure $ show i
repr (Float f) = pure $ case f of
  f
    | isInfinite f && f > 0 -> "inf"
    | isInfinite f && f < 0 -> "-inf"
    | isNaN f -> "nan"
    | otherwise -> show f
repr (String s) = pure $ show s
repr (Boolean b) = pure $ if b then "true" else "false"
repr Nil = pure "nil"
repr (Table t) = do
  items <- tableList t
  content <- intercalate ", " <$> mapM reprKV items
  return $ "table: {" ++ content ++ "}"
  where
    reprKV (k, v) = do
      k' <- reprK k
      v' <- repr v
      pure $ "[" ++ k' ++ "]" ++ " = " ++ v'
    reprK (TableKeyInt i) = pure $ show i
    reprK (TableKeyString s) = pure $ show s


data Table' = Table'
  { array :: TableArray,
    hash :: TableHash
  }

type TableArray = IORef (MV.IOVector Value)

type TableHash = HT.BasicHashTable TableKey Value

data TableKey
  = TableKeyInt Int64
  | TableKeyString BS.ByteString
  deriving (Eq, Generic)

instance Show TableKey where
  show (TableKeyInt i) = show i
  show (TableKeyString s) = show s

instance Hashable TableKey

emptyTable :: IO Table'
emptyTable = liftA2 Table' (newIORef =<< MV.new 0) HT.new

tableSet :: Table' -> TableKey -> Value -> IO ()
tableSet table key value = do
  case key of
    TableKeyInt i_ -> do
      let i = fromIntegral i_ - 1
      if i < 0
        then error "Index out of bounds"
        else do
          oldArray <- readIORef (array table)
          let oldLen = MV.length oldArray
          if i >= oldLen
            then do
              let newLen = (nextPowerOfTwo i) + 1
              newArray <- MV.grow oldArray (newLen - oldLen)
              MV.set (MV.slice oldLen (newLen - oldLen) newArray) Nil
              MV.write newArray i value
              writeIORef (array table) newArray
            else
              MV.write oldArray i value
    TableKeyString s -> HT.insert (hash table) key value
  where
    nextPowerOfTwo n = 2 ^ (ceiling $ logBase 2 (fromIntegral n) :: Int)

tableLen :: Table' -> IO Int64
tableLen t = do
  arrayLen <- MV.length <$> readIORef (array t)
  hashLen <- length <$> (HT.toList $ hash t)
  pure $ fromIntegral $ arrayLen + hashLen

tableGet :: Table' -> TableKey -> IO Value
tableGet table key = do
  case key of
    TableKeyInt i_ -> do
      let i = fromIntegral i_ - 1
      arr <- readIORef (array table)
      value <- MV.readMaybe arr i
      pure $ fromMaybe Nil value
    TableKeyString s -> do
      value <- HT.lookup (hash table) key
      pure $ fromMaybe Nil value

tableList :: Table' -> IO [(TableKey, Value)]
tableList table = do
  arrayValueList <- V.toList <$> (V.freeze =<< readIORef (array table))
  let arrayList = filter (nonNil . snd) $ zip (map TableKeyInt [1 ..]) arrayValueList
  hashList <- HT.toList (hash table)
  pure $ arrayList ++ hashList
  where
    nonNil Nil = False
    nonNil _ = True
