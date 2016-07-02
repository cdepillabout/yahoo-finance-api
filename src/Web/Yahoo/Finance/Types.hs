{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yahoo.Finance.Types where

import Data.Data (Data)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData(..))

newtype StockSymbol = StockSymbol { unStockSymbol :: Text }
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

instance ToHttpApiData StockSymbol where
    toUrlPiece :: StockSymbol -> Text
    toUrlPiece = unStockSymbol

instance ToHttpApiData [StockSymbol] where
    toUrlPiece :: [StockSymbol] -> Text
    toUrlPiece = fold . intersperse "," . fmap toUrlPiece

